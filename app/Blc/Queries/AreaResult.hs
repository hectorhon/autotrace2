module Blc.Queries.AreaResult
  ( AreaResult (AreaResult)
  , getAreaResult
  ) where

import Database.Esqueleto
import Data.Time
import Blc.Queries.DescendantBlcs
import Area.Types
import Blc.Types
import AppM
import Time

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

getAreaResult :: UTCTime -> UTCTime -> Key Area -> AppM (Maybe AreaResult)
getAreaResult start end aid = do
  mArea <- runDb (get aid)
  case mArea of
    Nothing -> return Nothing
    Just area -> do
      bids <- descendantBlcsOf aid
      results' <- runDb $ select $ from $ \ d -> do
        where_ (d ^. BlcResultDataBlc `in_` (valList bids)
                &&. d ^. BlcResultDataDay >=. (val $ utcToLocalDay start)
                &&. d ^. BlcResultDataDay <.  (val $ utcToLocalDay end))
        groupBy (d ^. BlcResultDataBlc)
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
      let dv a b = if b == 0 then 1 else (a :: Double) / (b :: Double)
      let results = (flip map) results' $
            \ (demand, uptimeDemand, uptime, performUptime,
               modeInterv, mvInterv, spInterv, mvSat, cvAffBySat) ->
              ((f uptimeDemand) `dv` (f demand),
               (f performUptime) `dv` (f uptime),
               f modeInterv, f mvInterv, f spInterv, f mvSat, f cvAffBySat)
      let complianceCount = foldr (\ (compliance,_,_,_,_,_,_) acc ->
            if compliance >= 0.95 then acc + 1 else acc) 0 results
      let qualityCount = foldr (\ (_,quality,_,_,_,_,_) acc ->
            if quality >= 0.95 then acc + 1 else acc) 0 results
      let modeInterv = truncate $
            foldr (\ (_,_,v,_,_,_,_) acc -> acc + v) 0 results
      let mvInterv = truncate $
            foldr (\ (_,_,_,v,_,_,_) acc -> acc + v) 0 results
      let spInterv = truncate $
            foldr (\ (_,_,_,_,v,_,_) acc -> acc + v) 0 results
      let totalDuration = diffUTCTime end start * realToFrac (length bids)
      let mvSat =
            if totalDuration == 0 then 0
            else (foldr (\ (_,_,_,_,_,v,_) acc -> acc + v) 0 results
                  / (realToFrac totalDuration))
      let cvAffBySat =
            if totalDuration == 0 then 0
            else (foldr (\ (_,_,_,_,_,_,v) acc -> acc + v) 0 results
                  / (realToFrac totalDuration))
      return $ Just (AreaResult (Entity aid area)
                                complianceCount qualityCount (length bids)
                                modeInterv mvInterv spInterv mvSat cvAffBySat)
