{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Blc.Queries 
  ( AreaResult (AreaResult)
  , BlcResult (BlcResult)
  , descendantBlcsOf
  , getResult
  , getBadCompliances
  , getBadQualities
  ) where

import Database.Esqueleto
import Data.List (sortOn)
import Data.Time
import Blc.Types
import Area.Types

data AreaResult =
  AreaResult (Entity Area)
             Int              -- ^ Compliance count
             Int              -- ^ Quality count
             Int              -- ^ Blc count
             Int              -- ^ Mode intervention
             Int              -- ^ MV intervention
             Int              -- ^ SP intervention
             Double           -- ^ MV saturation
             Double           -- ^ CV affected by saturation

data BlcResult =
  BlcResult (Entity Blc)
            (Maybe Double)  -- ^ Compliance
            (Maybe Double)  -- ^ Quality
            Int             -- ^ Mode intervention
            Int             -- ^ MV intervention
            Int             -- ^ SP intervention
            Double          -- ^ MV saturation
            Double          -- ^ CV affected by saturation

descendantBlcsOf :: Key Area -> SqlPersistT IO [Key Blc]
descendantBlcsOf area = rawSql
  " with recursive t as (                       \
  \   select * from area where id = ?           \
  \   union select area.* from area             \
  \         inner join t on area.parent = t.id) \
  \ select blc.id from blc inner join t on      \
  \ blc.area = t.id                             "
  [PersistInt64 $ fromSqlKey area]
  >>= return . map (toSqlKey . unSingle)

getBadCompliances :: Day -> Day -> Double -> [Key Blc]
                  -> SqlPersistT IO [(Entity Blc, Double)]
getBadCompliances start end complianceTarget bids = do
  results <- select $ from $ \ i -> do
    criteria i bids start end
    let uptimeDemand = coalesceDefault
                       [sum_ (i ^. BlcResultDataUptimeDemand)] (val 0)
    let demand = coalesceDefault [sum_ (i ^. BlcResultDataDemand)] (val 1)
    let compliance = case_ [ when_ (demand ==. val 0) then_ (val 1) ] $
                     else_ (uptimeDemand /. demand)
    having (compliance <. val complianceTarget)
    groupBy (i ^. BlcResultDataBlc)
    orderBy [ asc (i ^. BlcResultDataBlc) ]
    return (i ^. BlcResultDataBlc, compliance)
  let bids' = map (unValue . fst) results
  let compliances = map (unValue . snd) results
  blcs <- select $ from $ \ b -> do
    where_ (b ^. BlcId `in_` (valList bids'))
    orderBy [ asc (b ^. BlcId) ]
    return b
  return $ sortOn snd (zip blcs compliances)

getBadQualities :: Day -> Day -> Double -> [Key Blc]
                -> SqlPersistT IO [(Entity Blc, Double)]
getBadQualities start end qualityTarget bids = do
  results <- select $ from $ \ i -> do
    criteria i bids start end
    let performUptime = coalesceDefault
                        [sum_ (i ^. BlcResultDataPerformUptime)] (val 0)
    let uptime = coalesceDefault [sum_ (i ^. BlcResultDataUptime)] (val 1)
    let quality = case_ [ when_ (uptime ==. val 0) then_ (val 1) ] $
                  else_ (performUptime /. uptime)
    having (quality <. val qualityTarget)
    groupBy (i ^. BlcResultDataBlc)
    orderBy [ asc (i ^. BlcResultDataBlc) ]
    return (i ^. BlcResultDataBlc, quality)
  let bids' = map (unValue . fst) results
  let qualities = map (unValue . snd) results
  blcs <- select $ from $ \ b -> do
    where_ (b ^. BlcId `in_` (valList bids'))
    orderBy [ asc (b ^. BlcId) ]
    return b
  return $ sortOn snd (zip blcs qualities)

getResult :: Day -> Day -> Entity Area
          -> SqlPersistT IO (AreaResult, [AreaResult], [BlcResult])
getResult start end (Entity aid area) = do
  areaResult <- getAreaResult start end (Entity aid area)
  subareas <- select $ from $ \ i -> do
    where_ (i ^. AreaParent ==. val (Just aid))
    orderBy [asc (i ^. AreaName)]
    return i
  subareaResults <- mapM (getAreaResult start end) subareas
  blcResults <- getChildBlcResult start end aid
  return (areaResult, subareaResults, blcResults)

criteria :: ( Esqueleto query expr backend
            , PersistEntity BlcResultData
            , PersistEntityBackend BlcResultData ~ backend
            ) => expr (Entity BlcResultData)-> [Key Blc]-> Day-> Day-> query ()
criteria i bids start end = where_ (i ^. BlcResultDataBlc `in_` (valList bids)
                                    &&. i ^. BlcResultDataDay >=. (val start)
                                    &&. i ^. BlcResultDataDay <=. (val end))

-- Internal use, no completeness check
getAreaResult :: Day -> Day -> Entity Area -> SqlPersistT IO AreaResult
getAreaResult start end (Entity aid area) = do
  bids <- descendantBlcsOf aid
  complyCount <- fmap length $ select $ from $ \ i -> do
    criteria i bids start end
    let uptimeDemand = coalesceDefault
                       [sum_ (i ^. BlcResultDataUptimeDemand)]
                       (val (0 :: Double))
    let demand = coalesceDefault
                 [sum_ (i ^. BlcResultDataDemand)]
                 (val (0 :: Double))
    let compliance = case_ [ when_ (demand ==. val 0) then_ (val 1) ]
                           ( else_ $ uptimeDemand /. demand )
    having (compliance >=. val 0.95)
    groupBy (i ^. BlcResultDataBlc)
    return (i ^. BlcResultDataBlc)
  qualityCount <- fmap length $ select $ from $ \ i -> do
    criteria i bids start end
    let performUptime = coalesceDefault
                        [sum_ (i ^. BlcResultDataPerformUptime)]
                        (val (0 :: Double))
    let uptime = coalesceDefault
                 [sum_ (i ^. BlcResultDataUptime)]
                 (val (0 :: Double))
    let quality = case_ [ when_ (uptime ==. val 0) then_ (val 1) ]
                        ( else_ $ performUptime /. uptime )
    having (quality >=. val 0.95)
    groupBy (i ^. BlcResultDataBlc)
    return (i ^. BlcResultDataBlc)
  modeInterv <- select $ from $ \ i -> do
    criteria i bids start end
    return $ sum_ (i ^. BlcResultDataModeInterv)
  mvInterv <- select $ from $ \ i -> do
    criteria i bids start end
    return $ sum_ (i ^. BlcResultDataMvInterv)
  spInterv <- select $ from $ \ i -> do
    criteria i bids start end
    return $ sum_ (i ^. BlcResultDataSpInterv)
  let duration = val $ fromIntegral (length bids)
                       * fromIntegral (diffDays end start + 1)
                       * 86400
  mvSat <- select $ from $ \ i -> do
    criteria i bids start end
    let mvSat = coalesceDefault [sum_ $ i ^. BlcResultDataMvSat] (val 0)
    return $ case_ [ when_ (duration ==. val 0) then_ (val 0) ]
                   ( else_ $ mvSat /. duration )
  cvAffBySat <- select $ from $ \ i -> do
    criteria i bids start end
    let cvAffBySat= coalesceDefault [sum_$ i ^. BlcResultDataCvAffBySat] (val 0)
    return $ case_ [ when_ (duration ==. val 0) then_ (val 0) ]
                   ( else_ $ cvAffBySat /. duration )
  let f xs = if null xs then 0 else unValue (head xs)
  let g = maybe 0 (truncate :: Double -> Int) . unValue . head
  return $ AreaResult (Entity aid area) complyCount qualityCount
                      (length bids)
                      (g modeInterv) (g mvInterv) (g spInterv)
                      (f mvSat) (f cvAffBySat)

getChildBlcResult :: Day -> Day -> Key Area -> SqlPersistT IO [BlcResult]
getChildBlcResult start end aid = do
  results <- select $ from $ \ (i `RightOuterJoin` b) -> do
    on (i ^. BlcResultDataBlc ==. b ^. BlcId
        &&. i ^. BlcResultDataDay >=. (val start)
        &&. i ^. BlcResultDataDay <=. (val end))
    where_ (b ^. BlcArea ==. val aid)
    groupBy (b ^. BlcId)
    orderBy [asc (b ^. BlcId)]
    let uptimeDemand = coalesceDefault
                       [sum_ (i ^. BlcResultDataUptimeDemand)]
                       (val (0 :: Double))
    let demand = coalesceDefault
                 [sum_ (i ^. BlcResultDataDemand)]
                 (val (0 :: Double))
    let compliance = case_ [ when_ (demand ==. val 0) then_ nothing ]
                           ( else_ $ just $ uptimeDemand /. demand )
    let performUptime = coalesceDefault
                        [sum_ (i ^. BlcResultDataPerformUptime)]
                        (val (0 :: Double))
    let uptime = coalesceDefault
                 [sum_ (i ^. BlcResultDataUptime)]
                 (val (0 :: Double))
    let quality = case_ [ when_ (uptime ==. val 0) then_ nothing ]
                        ( else_ $ just $ performUptime /. uptime )
    let duration = val $ fromIntegral (diffDays end start + 1) * 86400
    let cvAffBySat= coalesceDefault [sum_$ i ^. BlcResultDataCvAffBySat] (val 0)
    let cvAffBySat' = case_ [ when_ (duration ==. val 0) then_ (val 0) ]
                            ( else_ $ cvAffBySat /. duration )
    let mvSat = coalesceDefault [sum_$ i ^. BlcResultDataMvSat] (val 0)
    let mvSat' = case_ [ when_ (duration ==. val 0) then_ (val 0) ]
                       ( else_ $ mvSat /. duration )
    return ( b ^. BlcId
           , compliance
           , quality
           , sum_ (i ^. BlcResultDataModeInterv)
           , sum_ (i ^. BlcResultDataMvInterv)
           , sum_ (i ^. BlcResultDataSpInterv)
           , mvSat'
           , cvAffBySat' )
  let t = maybe 0 (truncate :: Double -> Int)
  blcs <- select $ from $ \ i -> do
    let sel (Value a, _, _, _, _, _, _, _) = a
    where_ (i ^. BlcId `in_` valList (map sel results))
    orderBy [asc (i ^. BlcId)]
    return i
  let z (blc, (Value compliance, Value quality, Value modeInterv,
         Value mvInterv, Value spInterv, Value mvSat, Value cvAffBySat)) =
        BlcResult blc compliance quality
                  (t modeInterv) (t mvInterv) (t spInterv) mvSat cvAffBySat
  let sel (_, b, c, d, e, f, g, h) = (b, c, d, e, f, g, h)
  return $ map z $
           sortOn (blcName . entityVal . fst) $
           zip blcs (map sel results)
