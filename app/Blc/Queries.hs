{-# LANGUAGE OverloadedStrings #-}

module Blc.Queries where

import Database.Persist.Postgresql
import Data.Time
import Control.Monad
import Schema
import AppM

data AreaBlcResult =
  AreaBlcResult (Entity Area)
                Int              -- ^ Compliance count
                Int              -- ^ Quality count
                Int              -- ^ Blc count
                Int              -- ^ Mode intervention
                Int              -- ^ MV intervention
                Int              -- ^ SP intervention
                Double           -- ^ MV saturation
                Double           -- ^ CV affected by saturation
                [AreaBlcResult]
                [BlcResultSummary]

data BlcResultSummary =
  BlcResultSummary (Entity Blc)
            Double        -- ^ Compliance
            Double        -- ^ Quality
            Int           -- ^ Mode intervention
            Int           -- ^ MV intervention
            Int           -- ^ SP intervention
            Double        -- ^ MV saturation
            Double        -- ^ CV affected by saturation

blcResultOf :: Entity Area -> UTCTime -> UTCTime -> AppM AreaBlcResult
blcResultOf area start end =
  let duration = realToFrac (diffUTCTime end start) in do
    -- Subareas result
    children <- childAreasOf (entityKey area)
    blcss    <- mapM descendantBlcsOf (map entityKey children)
    resultsss <- liftM ((map . map . map) entityVal) $ forM blcss $
      (\ blcs -> forM blcs $
         (\ blc -> runDb $ selectList [ BlcResultBlc ==. blc
                                      , BlcResultStart >=. start
                                      , BlcResultEnd <=. end ] [] ))
    let compliances = (flip map) resultsss
          (\ resultss -> length $ filter (> 95) $ (flip map) resultss
             (\ results -> sum (map blcResultUptimeDemand results)
                           `d` sum (map blcResultDemand results)
                           * 100))
    let qualities = (flip map) resultsss
          (\ resultss -> length $ filter (> 95) $ (flip map) resultss
             (\ results -> sum (map blcResultPerformUptime results)
                           `d` sum (map blcResultUptime results)
                           * 100))
    let blcCounts = map length blcss
    modeIntervCounts <- mapM (numEvents ModeInterv start end) blcss
    mvIntervCounts <- mapM (numEvents MvInterv start end) blcss
    spIntervCounts <- mapM (numEvents SpInterv start end) blcss
    let durations = map ((* duration) . realToFrac . length) blcss
    let mvSats = (flip map) resultsss
          (\ resultss -> sum $ zipWith (flip d) durations $
             (flip map) resultss
               (\ results -> sum (map blcResultMvSat results)))
    let cvAffBySats = (flip map) resultsss
          (\ resultss -> sum $ zipWith (flip d) durations $
             (flip map) resultss
               (\ results -> sum (map blcResultCvAffBySat results)))
    -- Blcs here
    blcs <- childBlcsOf (entityKey area)
    resultss <- liftM ((map . map) entityVal) $ forM blcs $
      (\ blc -> runDb $ selectList [ BlcResultBlc ==. (entityKey blc)
                                   , BlcResultStart >=. start
                                   , BlcResultEnd <=. end ] [] )
    let compliances' = (flip map) resultss
          (\ results -> sum (map blcResultUptimeDemand results)
                        `d` sum (map blcResultDemand results)
                        * 100)
    let qualities' = (flip map) resultss
          (\ results -> sum (map blcResultPerformUptime results)
                        `d` sum (map blcResultUptime results)
                        * 100)
    modeIntervCounts' <- forM blcs
                         (numEvents ModeInterv start end . return . entityKey)
    mvIntervCounts' <- forM blcs
                       (numEvents MvInterv start end . return . entityKey)
    spIntervCounts' <- forM blcs
                       (numEvents SpInterv start end . return . entityKey)
    let mvSats' = (flip map) resultss
          (\ results -> sum (map blcResultMvSat results) `d` duration)
    let cvAffBySats' = (flip map) resultss
          (\ results -> sum (map blcResultCvAffBySat results) `d` duration)
    -- Area summary
    let compliance = sum compliances + length (filter (>95) compliances')
    let quality = sum qualities + length (filter (>95) qualities')
    let blcCount = sum blcCounts + length blcs
    let modeIntervCount = sum modeIntervCounts + sum modeIntervCounts'
    let mvIntervCount = sum mvIntervCounts + sum mvIntervCounts'
    let spIntervCount = sum spIntervCounts + sum spIntervCounts'
    let duration' = sum durations + duration * (realToFrac $ length blcs)
    let mvSat = (sum (zipWith (*) durations mvSats)
                 + sum (map (* duration) mvSats'))
                `d` duration'
    let cvAffBySat = (sum (zipWith (*) durations cvAffBySats)
                      + sum (map (* duration) cvAffBySats'))
                     `d` duration'
    -- Zip it up
    return $ AreaBlcResult area
                           compliance quality blcCount
                           modeIntervCount mvIntervCount spIntervCount
                           mvSat cvAffBySat
                           (zipWith11 AreaBlcResult children
                                                    compliances qualities
                                                    blcCounts
                                                    modeIntervCounts
                                                    mvIntervCounts
                                                    spIntervCounts
                                                    mvSats cvAffBySats
                                                    (repeat []) (repeat []))
                           (zipWith8 BlcResultSummary blcs
                                               compliances' qualities'
                                               modeIntervCounts'
                                               mvIntervCounts'
                                               spIntervCounts'
                                               mvSats' cvAffBySats')

-- |Defaults to zero on divide by zero
d :: Double -> Double -> Double
d a b = if b == 0 then 0 else a / b

childAreasOf :: Key Area -> AppM [Entity Area]
childAreasOf area = runDb $ selectList [AreaParent ==. Just area] [Asc AreaName]

descendantBlcsOf :: Key Area -> AppM [Key Blc]
descendantBlcsOf area = liftM (map (toSqlKey . unSingle)) $ runDb $ rawSql
  " with recursive t as (\
  \   select * from area where id = ?\
  \   union select area.* from area\
  \         inner join t on area.parent = t.id)\
  \ select blc.id from blc inner join t on\
  \ blc.area = t.id "
  [PersistInt64 $ fromSqlKey area]

descendantBlcsOf' :: Key Area -> AppM [Entity Blc]
descendantBlcsOf' area = runDb $ rawSql
  " with recursive t as (\
  \   select * from area where id = ?\
  \   union select area.* from area\
  \         inner join t on area.parent = t.id)\
  \ select ?? from blc inner join t on\
  \ blc.area = t.id "
  [PersistInt64 $ fromSqlKey area]

childBlcsOf :: Key Area -> AppM [Entity Blc]
childBlcsOf area = runDb $ selectList [BlcArea ==. area] [Asc BlcName]

numEvents :: EventType -> UTCTime -> UTCTime -> [Key Blc] -> AppM Int
numEvents eventType start end blcs = runDb $
  count [ BlcEventTime >=. start
        , BlcEventTime <=. end
        , BlcEventBlc <-. blcs
        , BlcEventCategory ==. eventType ]

zipWith8 :: (a -> b -> c -> d -> e -> f -> g -> h -> z)
         -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [h] -> [z]
zipWith8 fx c1 c2 c3 c4 c5 c6 c7 c8 =
  if (   null c1 || null c2 || null c3 || null c4 || null c5
      || null c6 || null c7 || null c8                       ) then []
  else (fx (head c1) (head c2) (head c3) (head c4) (head c5)
           (head c6) (head c7) (head c8))
         :
         (zipWith8 fx (tail c1) (tail c2) (tail c3) (tail c4) (tail c5)
                      (tail c6) (tail c7) (tail c8))

zipWith11 :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> z)
          -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [h] -> [i] -> [j] 
          -> [k] -> [z]
zipWith11 fx c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 =
  if (   null c1 || null c2 || null c3 || null c4 || null c5
      || null c6 || null c7 || null c8 || null c9 || null c10
      || null c11)
  then []
  else (fx (head c1) (head c2) (head c3) (head c4) (head c5)
           (head c6) (head c7) (head c8) (head c9) (head c10)
           (head c11))
         :
         (zipWith11 fx (tail c1) (tail c2) (tail c3) (tail c4) (tail c5)
                       (tail c6) (tail c7) (tail c8) (tail c9) (tail c10)
                       (tail c11))
