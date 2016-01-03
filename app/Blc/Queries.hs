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
                [BlcResult]

data BlcResult =
  BlcResult (Entity Blc)
            Double        -- ^ Compliance
            Double        -- ^ Quality
            Int           -- ^ Mode intervention
            Int           -- ^ MV intervention
            Int           -- ^ SP intervention
            Double        -- ^ MV saturation
            Double        -- ^ CV affected by saturation

blcResultOf :: Entity Area -> UTCTime -> UTCTime -> AppM AreaBlcResult
blcResultOf area start end = let duration = diffUTCTime end start in do
  -- Subareas result
  children             <- childAreasOf (entityKey area)
  blcss                <- mapM descendantBlcsOf (map entityKey children)
  intervalsss          <- mapM (mapM (intervalsDuring' start end)) blcss
  let compliancess     = map (map complianceOf) intervalsss
  let compliances      = map (length . filter (> 95)) compliancess
  let qualitiess       = map (map qualityOf) intervalsss
  let qualities        = map (length . filter (> 95)) qualitiess
  let blcCounts        = map length blcss
  let intervalss       = map concat intervalsss
  let modeIntervCounts = map (modeIntervCountDuring start end) intervalss
  mvIntervCounts       <- mapM (numEvents MvInterv start end) blcss
  spIntervCounts       <- mapM (numEvents SpInterv start end) blcss
  let durations        = map ((* duration) . realToFrac . length) blcss
  let mvSats           = zipWith mvSatOf durations intervalss
  let cvAffBySats      = zipWith cvAffBySatOf durations intervalss
  -- Blcs here
  blcs                  <- childBlcsOf (entityKey area)
  intervalss'           <- mapM (intervalsDuring' start end . entityKey) blcs
  let compliances'      = map complianceOf intervalss'
  let qualities'        = map qualityOf intervalss'
  let modeIntervCounts' = map (modeIntervCountDuring start end) intervalss'
  mvIntervCounts' <- mapM (numEvents MvInterv start end . return.entityKey) blcs
  spIntervCounts' <- mapM (numEvents SpInterv start end . return.entityKey) blcs
  let mvSats'           = map (mvSatOf duration) intervalss'
  let cvAffBySats'      = map (cvAffBySatOf duration) intervalss'
  -- Area summary
  let intervalss''    = (concat intervalsss) ++ intervalss'
  let compliance      = length $ filter (> 95) $ map complianceOf intervalss''
  let quality         = length $ filter (> 95) $ map qualityOf intervalss''
  let blcCount        = length intervalss''
  let intervals'      = concat intervalss''
  let modeIntervCount = modeIntervCountDuring start end intervals'
  let blcs'           = concat blcss ++ (map entityKey blcs)
  mvIntervCount       <- numEvents MvInterv start end blcs'
  spIntervCount       <- numEvents SpInterv start end blcs'
  let duration'       = duration * realToFrac (length blcs')
  let mvSat           = mvSatOf duration' intervals'
  let cvAffBySat      = cvAffBySatOf duration' intervals'
  -- Zip it up
  return $ AreaBlcResult area
                         compliance quality blcCount
                         modeIntervCount mvIntervCount spIntervCount
                         mvSat cvAffBySat
                         (zipWith11 AreaBlcResult children
                                                  compliances qualities
                                                  blcCounts
                                                  modeIntervCounts
                                                  mvIntervCounts spIntervCounts
                                                  mvSats cvAffBySats
                                                  (repeat []) (repeat []))
                         (zipWith8 BlcResult blcs
                                             compliances' qualities'
                                             modeIntervCounts'
                                             mvIntervCounts'
                                             spIntervCounts'
                                             mvSats' cvAffBySats')

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

intervalsDuring' :: UTCTime -> UTCTime -> Key Blc -> AppM [BlcInterval]
intervalsDuring' start end blc =
  let timeFilter = (    [BlcIntervalStart >=. start, BlcIntervalStart <.  end]
                    ||. [BlcIntervalEnd   >.  start, BlcIntervalEnd   <=. end]
                    ||. [BlcIntervalStart <=. start, BlcIntervalEnd   >=. end])
  in liftM (map (trim . entityVal)) $ runDb $ selectList
       ([ BlcIntervalBlc ==. blc ] ++ timeFilter) []
  where trim (BlcInterval bid start' end' category) =
          BlcInterval bid (max start' start) (min end' end) category

complianceOf :: [BlcInterval] -> Double
complianceOf blcis =
  if totalDurationOf demand == 0 then 0
  else realToFrac $ totalDurationOf uptimeDemand / totalDurationOf demand * 100
  where uptimeDemand = filter ((== UptimeDemand) . blcIntervalCategory) blcis
        demand = filter ((== Demand) . blcIntervalCategory) blcis

qualityOf :: [BlcInterval] -> Double
qualityOf blcis =
  if totalDurationOf uptime == 0 then 0
  else realToFrac $ totalDurationOf performUptime / totalDurationOf uptime * 100
  where performUptime = filter ((== PerformUptime) . blcIntervalCategory) blcis
        uptime = filter ((== Uptime) . blcIntervalCategory) blcis

durationOf :: BlcInterval -> NominalDiffTime
durationOf (BlcInterval _ start end _) = diffUTCTime end start

totalDurationOf :: [BlcInterval] -> NominalDiffTime
totalDurationOf = foldr ((+) . durationOf) 0

numEvents :: EventType -> UTCTime -> UTCTime -> [Key Blc] -> AppM Int
numEvents eventType start end blcs = runDb $
  count [ BlcEventTime >=. start
        , BlcEventTime <=. end
        , BlcEventBlc <-. blcs
        , BlcEventCategory ==. eventType ]

modeIntervCountDuring :: UTCTime -> UTCTime -> [BlcInterval] -> Int
modeIntervCountDuring _ end = length . filter
  (\ i -> blcIntervalCategory i == Uptime && blcIntervalEnd i /= end)

mvSatOf :: NominalDiffTime -> [BlcInterval] -> Double
mvSatOf totalDuration blcis =
  if totalDuration == 0 then 0
  else realToFrac $ totalDurationOf mvSat / totalDuration * 100
  where mvSat = filter ((== MvSat) . blcIntervalCategory) blcis

cvAffBySatOf :: NominalDiffTime -> [BlcInterval] -> Double
cvAffBySatOf totalDuration blcis =
  if totalDuration == 0 then 0
  else realToFrac $ totalDurationOf cvAffBySat / totalDuration * 100
  where cvAffBySat = filter ((== CvAffBySat) . blcIntervalCategory) blcis

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
