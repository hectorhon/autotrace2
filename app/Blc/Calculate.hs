{-# LANGUAGE OverloadedStrings #-}

module Blc.Calculate (
  job
  ) where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Concurrent
import Database.Persist.Postgresql
import Data.Tree
import Data.List (groupBy, sortOn, nub)
import qualified Data.Text as T (intercalate, pack, concat)
import Control.Exception (assert)
import Area.Types
import Blc.Types
import TimeSeriesData
import Time

job :: Day -> Day -> [Key Blc]
    -> ReaderT (String, Int, SqlBackend, MVar (Int, Int)) IO ()
job start end bids = do
  (_, _, db, progress) <- ask
  (lareas, blcs) <- liftIO $ (flip runSqlConn) db $ do
    blcs <- selectList [BlcId <-. bids] []
    lareas <- listAreas blcs
    return (lareas, blcs)
  _ <- liftIO (swapMVar progress (0, length blcs))
  (results, _) <- withReaderT (\ (s, p, _, _) -> (s, p))
    (runWriterT $ calculateForest start end (buildForest lareas blcs))
  let store = mapM_ $ \ result -> do
        deleteWhere [ BlcResultDataBlc ==. blcResultDataBlc result
                    , BlcResultDataDay >=. start
                    , BlcResultDataDay <=. end ]
        insert_ result
        liftIO (modifyMVar_ progress (\ (p, x) -> return (p + 1, x)))
  liftIO (runSqlConn (store results) db)

listAreas :: [Entity Blc] -> SqlPersistT IO [(Int, Entity Area)]
listAreas blcs = do
  rawSql stmt [] >>= return . toAreaEntity
  where keys = T.intercalate ","
               (map (T.pack . show . fromSqlKey. blcArea . entityVal) blcs)
        stmt = T.concat [
          " with recursive t as (                                      \
          \   select (0) as level,* from area where id in (", keys, ") \
          \   union select level-1,area.* from area                    \
          \   inner join t on area.id = t.parent) select * from t;     "]
        toAreaEntity = map
          (\ (Single l, Single i, Single n, Single d, Single p, Single dc) ->
             (l, Entity i (Area n d p dc)))

buildForest :: [(Int, Entity Area)] -> [Entity Blc]
            -> Forest (Entity Area, [Entity Blc])
buildForest areas blcs = unfoldForest builder seeds
  where seeds = case groupBy sameLevel (sortOn fst areas) of
                  [] -> []
                  xs -> map snd (head xs)
        sameLevel a b = fst a == fst b
        builder area = let label = (area, filter (isChildBlcOf area) blcs)
                           childs = filter (isChildAreaOf area) (map snd areas)
                       in (label, childs)
        isChildBlcOf area = (== (entityKey area)) . blcArea . entityVal
        isChildAreaOf area = maybe False (== (entityKey area))
                             . areaParent . entityVal

calculateForest :: Day -> Day -> Forest (Entity Area, [Entity Blc])
                -> WriterT [String] (ReaderT (String, Int) IO) [BlcResultData]
calculateForest start end forest = do
  fmap concat (mapM (calculateTree [(startNDT, endNDT)] start end) forest)
  where startNDT = diffUTCTime (localDayToUTC start) refTime
        endNDT = diffUTCTime (localDayToUTC (addDays 1 end)) refTime

calculateTree :: [TSInterval] -> Day -> Day -> Tree (Entity Area, [Entity Blc])
                 -> WriterT [String] (ReaderT (String, Int) IO) [BlcResultData]
calculateTree ancestorDemand start end tree = do
  localDemand <- calculateAreaDemand (localDayToUTC start)
                                     (localDayToUTC (addDays 1 end))
                                     (entityVal (fst (rootLabel tree)))
  let areaDemand = filterCounts 2 [localDemand, ancestorDemand]
  blcResults <- mapM (calculateBlc start end areaDemand) (snd (rootLabel tree))
  moreBlcResults <- mapM (calculateTree areaDemand start end) (subForest tree)
  return (concat blcResults ++ concat moreBlcResults) 

calculateAreaDemand :: UTCTime -> UTCTime -> Area
                    -> WriterT [String] (ReaderT (String, Int) IO) [TSInterval]
calculateAreaDemand start end area =
  case parseExpression (areaDemandCond area) of
    Left err -> tell [show err] >> return []
    Right condition -> do
      (source, port) <- ask
      tsData <- liftIO (getTSData source port (listTags condition) start end)
      return (evaluate condition tsData)

calculateBlc :: Day -> Day -> [TSInterval] -> Entity Blc
             -> WriterT [String] (ReaderT (String, Int) IO) [BlcResultData]
calculateBlc start end areaDemand blc = do
  (source, port) <- ask
  let (parsedBlc, parseLog) = runWriter (parseBlc blc)
  tell parseLog
  tsData <- liftIO (getTSData source port (listBlcTags parsedBlc)
                              (localDayToUTC start)
                              (localDayToUTC (addDays 1 end)))
  -- TODO: tell missing tags
  return (calculateBlc' parsedBlc tsData areaDemand)

calculateBlc' :: ParsedBlc -> TSData -> [TSInterval] -> [BlcResultData]
calculateBlc' (ParsedBlc bid _ _ _ measTag sptTag outTag
               outMax outMin demandCond uptimeCond objective margin
               calcMvICond calcSpICond) tsData areaDemand =
  zipWith10' (BlcResultData bid) days (sumByDay days demand)
                                      (sumByDay days uptimeDemand)
                                      (sumByDay days uptime)
                                      (sumByDay days performUptime)
                                      (sumByDay' days modeInterv)
                                      (sumByDay' days mvInterv)
                                      (sumByDay' days spInterv)
                                      (sumByDay days mvSat)
                                      (sumByDay days cvAffBySat)
  where startDay = utcToLocalDay (addUTCTime (startOf tsData) refTime)
        endDay = utcToLocalDay (addUTCTime (endOf tsData) refTime)
        -- TODO: midnight round up
        days = [startDay..addDays (-1) endDay]
        demand = filterCounts 2 [evaluate demandCond tsData, areaDemand]
        uptime = evaluate uptimeCond tsData
        uptimeDemand = filterCounts 2 [uptime, demand]
        performCond = case objective of
          Near -> TknAnd
            (TknLessThan (TknTagName measTag)
                         (TknAdd (TknTagName sptTag) (TknNumber margin)))
            (TknMoreThan (TknTagName measTag)
                         (TknMinus (TknTagName sptTag) (TknNumber margin)))
          Above -> TknLessThan
            (TknMinus (TknTagName sptTag) (TknTagName measTag))
            (TknNumber margin)
          Below -> TknLessThan
            (TknMinus (TknTagName measTag) (TknTagName sptTag))
            (TknNumber margin)
        perform = evaluate performCond tsData
        performUptime = filterCounts 2 [perform, uptime]
        modeInterv = filter (/= (endOf tsData)) (map snd uptime)
        calcMvI = evaluate calcMvICond tsData
        mvInterv = case (lookup outTag (dataOf tsData)) of
          Nothing -> []
          Just tagData -> changesIn tagData calcMvI
        calcSpI = evaluate calcSpICond tsData
        spInterv = case (lookup sptTag (dataOf tsData)) of
          Nothing -> []
          Just tagData -> changesIn tagData calcSpI
        mvSatCond = TknOr
          (TknMoreEqThan (TknTagName outTag) (TknNumber outMax))
          (TknLessEqThan (TknTagName outTag) (TknNumber outMin))
        mvSat = evaluate mvSatCond tsData
        notPerform = filterCounts 0 [perform]
        cvAffBySat = filterCounts 2 [notPerform, mvSat]

data ParsedBlc = ParsedBlc (Key Blc) String String (Key Area)
                           String String String
                           Double Double Expression Expression Objective Double
                           Expression Expression

parseBlc :: Entity Blc -> Writer [String] ParsedBlc
parseBlc (Entity bid blc) = do
  let name = blcName blc
  let description = blcDescription blc
  let area = blcArea blc
  let measTag = blcMeasTag blc
  let sptTag = blcSptTag blc
  let outTag = blcOutTag blc
  let outMax = blcOutMax blc
  let outMin = blcOutMin blc
  demandCond <- parseExpression' (blcDemandCond blc)
  uptimeCond <- parseExpression' (blcUptimeCond blc)
  let objective = blcObjective blc
  let margin = blcMargin blc
  calcMvICond <- parseExpression' (blcCalcMvICond blc)
  calcSpICond <- parseExpression' (blcCalcSpICond blc)
  return (ParsedBlc bid name description area measTag sptTag outTag
                    outMax outMin demandCond uptimeCond objective
                    margin calcMvICond calcSpICond)

parseExpression' :: String -> Writer [String] Expression
parseExpression' condition = case parseExpression condition of
  Left err -> tell [show err] >> return TknInvalid
  Right condition' -> return condition'

listBlcTags :: ParsedBlc -> [String]
listBlcTags (ParsedBlc _ _ _ _ measTag sptTag outTag
                       _ _ demandCond uptimeCond _ _ calcMvICond calcSpICond) =
  nub (concat [ [ measTag, sptTag, outTag ]
              , listTags demandCond, listTags uptimeCond
              , listTags calcMvICond, listTags calcSpICond ])

filterCounts :: Int -> [[TSInterval]] -> [TSInterval]
filterCounts n = map fst . filter ((== n) . snd) . counts

zipWith10' :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> z)
           -> [a] -> [b] -> [c] -> [d] -> [e]
           -> [f] -> [g] -> [h] -> [i] -> [j] -> [z]
zipWith10' fx c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 = assert
  (   length c1 == length c2 && length c2 == length c3
   && length c3 == length c4 && length c4 == length c5
   && length c5 == length c6 && length c6 == length c7
   && length c7 == length c8 && length c8 == length c9
   && length c9 == length c10)
  (zipWith10 fx c1 c2 c3 c4 c5 c6 c7 c8 c9 c10)

zipWith10 :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> z)
         -> [a] -> [b] -> [c] -> [d] -> [e]
         -> [f] -> [g] -> [h] -> [i] -> [j] -> [z]
zipWith10 fx c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 =
  if (   null c1 || null c2 || null c3 || null c4 || null c5
      || null c6 || null c7 || null c8 || null c9 || null c10 ) then []
  else (fx (head c1) (head c2) (head c3) (head c4) (head c5)
           (head c6) (head c7) (head c8) (head c9) (head c10))
         :
         (zipWith10 fx (tail c1) (tail c2) (tail c3) (tail c4) (tail c5)
                       (tail c6) (tail c7) (tail c8) (tail c9) (tail c10))
