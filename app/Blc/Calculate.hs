module Blc.Calculate 
  ( markCalculate
  ) where

import TimeSeriesData
import Database.Persist.Postgresql
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Concurrent
import Control.Exception (assert)
import Data.ByteString.Char8 (ByteString)
import AppM
import Config
import Blc.Types
import Time
import Area.Queries.IntervalsDuring

data CalcOpts = CalcOpts String Int Day Day

markCalculate :: Day -> Day -> Key Blc -> AppM ()
markCalculate start end bid = do
  qsemn   <- asks getQSemN
  counter <- asks getCounter
  src     <- asks getSrcUrl
  port    <- asks getSrcPort
  connStr <- asks getPoolConnStr
  let calcOpts = CalcOpts src port start end
  mBlc <- runDb (get bid)
  case mBlc of
    Nothing -> return ()
    Just blc -> do
      let eb = Entity bid blc
      _ <- liftIO $ forkIO $ calculateThread qsemn counter calcOpts connStr eb
      return ()

calculateThread :: QSemN -> MVar Int -> CalcOpts -> ByteString -> Entity Blc
                -> IO ()
calculateThread qsemn counter calcOpts connStr (Entity kBlc blc) = do
  -- Acquire lock
  waitQSemN qsemn 1
  modifyMVar_ counter (return . (+) 1)
  -- Get area demand state
  let CalcOpts _ _ start end = calcOpts
  areaDemand <- runNoLoggingT $ withPostgresqlConn connStr $ lift . runReaderT
    (intervalsDuring (localDayToUTC start)
                     (localDayToUTC (addDays 1 end))
                     (blcArea blc))
  -- Perform the calculations
  results <- (flip runReaderT) calcOpts $ do
    let filterCounts n = map fst . filter ((== n) . snd)
    demand' <- calcInterval (blcDemandCond blc)
    let demand = filterCounts 2 $ counts [areaDemand, demand']
    uptime <- calcInterval (blcUptimeCond blc)
    let uptimeDemand = filterCounts 2 $ counts [demand, uptime]
    perform <- calcInterval $
      let margin  = show $ blcMargin blc
          measTag = blcMeasTag blc
          sptTag  = blcSptTag blc
      in case blcObjective blc of
           Near  -> concat ["[", measTag, "]<[", sptTag, "]+", margin ," and "
                           ,"[", measTag, "]>[", sptTag, "]-", margin]
           Above -> concat ["[", sptTag,  "]-[", measTag, "]<", margin]
           Below -> concat ["[", measTag, "]-[", sptTag,  "]<", margin]
    let performUptime = filterCounts 2 $ counts [perform, uptime]
    let modeInterv = map snd uptime
    calcMvI  <- calcInterval (blcCalcMvICond blc)
    calcSpI  <- calcInterval (blcCalcSpICond blc)
    mvInterv <- calcChange (blcOutTag blc) calcMvI
    spInterv <- calcChange (blcSptTag blc) calcSpI
    mvSat    <- calcInterval $
                let outTag = blcOutTag blc
                    outMax = show (blcOutMax blc)
                    outMin = show (blcOutMin blc)
                in concat [ "[", outTag, "]>=", outMax, " or "
                          , "[", outTag, "]<=", outMin ]
    let notPerform = filterCounts 0 $ counts [perform]
    let cvAffBySat = filterCounts 2 $ counts [notPerform, mvSat]
    let days = [start .. end]
    return $ zipWith10' (BlcResultData kBlc) days
                                             (sumByDay days demand)
                                             (sumByDay days uptimeDemand)
                                             (sumByDay days uptime)
                                             (sumByDay days performUptime)
                                             (sumByDay' days modeInterv)
                                             (sumByDay' days mvInterv)
                                             (sumByDay' days spInterv)
                                             (sumByDay days mvSat)
                                             (sumByDay days cvAffBySat)
  -- Write to database
  runNoLoggingT $ withPostgresqlConn connStr $ lift . runReaderT (do
    deleteWhere [ BlcResultDataBlc ==. kBlc
                , BlcResultDataDay >=. start
                , BlcResultDataDay <=. end ]
    insertMany_ results)
  -- Release lock
  modifyMVar_ counter (return . (flip (-)) 1)
  signalQSemN qsemn 1

calcInterval :: String -> ReaderT CalcOpts IO [TSInterval]
calcInterval condition = case parseExpression condition of
  Left  _          -> return []
  Right condition' -> do
    CalcOpts src port start end <- ask
    let start' = localDayToUTC start
    let end'   = localDayToUTC (addDays 1 end)
    tsData <- lift $ getTSData src port (listTags condition') start' end'
    return (evaluate condition' tsData)

calcChange :: String -> [TSInterval] -> ReaderT CalcOpts IO [NominalDiffTime]
calcChange tagName intervals = do
  CalcOpts src port start end <- ask
  let start' = localDayToUTC start
  let end'   = localDayToUTC (addDays 1 end)
  tsPoints <- lift $ getTSPoints src port start' end' tagName
  return (changesIn tsPoints intervals)

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
