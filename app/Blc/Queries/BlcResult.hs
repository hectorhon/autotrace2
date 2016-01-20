{-# LANGUAGE FlexibleContexts #-}

module Blc.Queries.BlcResult 
  ( BlcResult (BlcResult)
  , getChildBlcResult
  ) where

import Database.Esqueleto
import Data.Time
import AppM
import Area.Types
import Blc.Types
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
         orderBy [asc (b ^. BlcName)]
         return ( b
                , sum_ (d ^. BlcResultDataDemand)
                , sum_ (d ^. BlcResultDataUptimeDemand)
                , sum_ (d ^. BlcResultDataUptime)
                , sum_ (d ^. BlcResultDataPerformUptime)
                , sum_ (d ^. BlcResultDataModeInterv)
                , sum_ (d ^. BlcResultDataMvInterv)
                , sum_ (d ^. BlcResultDataSpInterv)
                , sum_ (d ^. BlcResultDataMvSat)
                , sum_ (d ^. BlcResultDataCvAffBySat))
    let f (Value mx) = maybe 0 id mx
    let dv a b = if b == 0 then Nothing else Just (a / b)
    let g ( blc, demand, uptimeDemand, uptime, performUptime
          , modeInterv, mvInterv, spInterv, mvSat, cvAffBySat ) =
          BlcResult blc ((f uptimeDemand) `dv` (f demand))
                        ((f performUptime) `dv` (f uptime))
                        (f modeInterv) (f mvInterv) (f spInterv)
                        (f mvSat) (f cvAffBySat)
    return $ map g results
