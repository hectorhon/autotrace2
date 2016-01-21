{-# LANGUAGE FlexibleContexts #-}

module Blc.Queries.BlcResult 
  ( BlcResult (BlcResult)
  , getChildBlcResult
  , getBadCompliances
  , getBadQualities
  ) where

import Database.Esqueleto
import Data.Time
import Data.List (sortOn)
import Area.Types
import Blc.Types
import AppM
import Time

data BlcResult =
  BlcResult (Entity Blc)
            (Maybe Double)  -- ^ Compliance
            (Maybe Double)  -- ^ Quality
            Int             -- ^ Mode intervention
            Int             -- ^ MV intervention
            Int             -- ^ SP intervention
            Double          -- ^ MV saturation
            Double          -- ^ CV affected by saturation

getChildBlcResult :: UTCTime -> UTCTime -> Key Area -> AppM [BlcResult]
getChildBlcResult start end aid = do
    blcs <- runDb $ select $ from $ \ i -> do
      where_ (i ^. BlcArea ==. val aid)
      orderBy [asc (i ^. BlcId)]
      return i
    let bids = map entityKey blcs
    results <- runDb $ select $ from $
       \ (b `LeftOuterJoin` d) -> do
         on (b ^. BlcId ==. d ^. BlcResultDataBlc)
         where_ (d ^. BlcResultDataBlc `in_` (valList bids)
                 &&. d ^. BlcResultDataDay >=. (val $ utcToLocalDay start)
                 &&. d ^. BlcResultDataDay <.  (val $ utcToLocalDay end))
         groupBy (d ^. BlcResultDataBlc)
         orderBy [asc (d ^. BlcResultDataBlc)]
         return ( sum_ (d ^. BlcResultDataDemand)
                , sum_ (d ^. BlcResultDataUptimeDemand)
                , sum_ (d ^. BlcResultDataUptime)
                , sum_ (d ^. BlcResultDataPerformUptime)
                , sum_ (d ^. BlcResultDataModeInterv)
                , sum_ (d ^. BlcResultDataMvInterv)
                , sum_ (d ^. BlcResultDataSpInterv)
                , sum_ (d ^. BlcResultDataMvSat)
                , sum_ (d ^. BlcResultDataCvAffBySat))
    let f (Value mx) = maybe (0 :: Double) id mx
    let dv a b = if b == 0 then Nothing else Just (a / b)
    let totalDuration = realToFrac (diffUTCTime end start)
    let g ( blc, (demand, uptimeDemand, uptime, performUptime
          , modeInterv, mvInterv, spInterv, mvSat, cvAffBySat) ) =
          BlcResult blc ((f uptimeDemand) `dv` (f demand))
                        ((f performUptime) `dv` (f uptime))
                        (truncate $ f modeInterv)
                        (truncate $ f mvInterv)
                        (truncate $ f spInterv)
                        (f mvSat / totalDuration)
                        (f cvAffBySat / totalDuration)
    return $ map g (zip blcs results)

getBadCompliances :: UTCTime -> UTCTime -> Double -> [Key Blc]
                  -> AppM [(Entity Blc, Double)]
getBadCompliances start end complianceTarget bids = do
  results <- runDb $ select $ from $ \ d -> do
    let uptimeDemand =
          coalesceDefault [sum_ (d ^. BlcResultDataUptimeDemand)] (val 0)
    let demand = coalesceDefault [sum_ (d ^. BlcResultDataDemand)] (val 1)
    let compliance = case_ [ when_ (demand ==. val 0) then_ (val 1) ] $
                     else_ (uptimeDemand /. demand)
    where_ (d ^. BlcResultDataBlc `in_` (valList bids)
            &&. d ^. BlcResultDataDay >=. (val $ utcToLocalDay start)
            &&. d ^. BlcResultDataDay <.  (val $ utcToLocalDay end))
    having (compliance <. val complianceTarget)
    groupBy (d ^. BlcResultDataBlc)
    orderBy [ asc (d ^. BlcResultDataBlc) ]
    return (d ^. BlcResultDataBlc, compliance)
  let bids' = map (unValue . fst) results
  let compliances = map (unValue . snd) results
  blcs <- runDb $ select $ from $ \ b -> do
    where_ (b ^. BlcId `in_` (valList bids'))
    orderBy [ asc (b ^. BlcId) ]
    return b
  return $ sortOn snd (zip blcs compliances)

getBadQualities :: UTCTime -> UTCTime -> Double -> [Key Blc]
                -> AppM [(Entity Blc, Double)]
getBadQualities start end qualityTarget bids = do
  results <- runDb $ select $ from $ \ d -> do
    let performUptime =
          coalesceDefault [sum_ (d ^. BlcResultDataPerformUptime)] (val 0)
    let uptime = coalesceDefault [sum_ (d ^. BlcResultDataUptime)] (val 1)
    let quality = case_ [ when_ (uptime ==. val 0) then_ (val 1) ] $
                     else_ (performUptime /. uptime)
    where_ (d ^. BlcResultDataBlc `in_` (valList bids)
            &&. d ^. BlcResultDataDay >=. (val $ utcToLocalDay start)
            &&. d ^. BlcResultDataDay <.  (val $ utcToLocalDay end))
    having (quality <. val qualityTarget)
    groupBy (d ^. BlcResultDataBlc)
    orderBy [ asc (d ^. BlcResultDataBlc) ]
    return (d ^. BlcResultDataBlc, quality)
  let bids' = map (unValue . fst) results
  let qualities = map (unValue . snd) results
  blcs <- runDb $ select $ from $ \ b -> do
    where_ (b ^. BlcId `in_` (valList bids'))
    orderBy [ asc (b ^. BlcId) ]
    return b
  return $ sortOn snd (zip blcs qualities)
