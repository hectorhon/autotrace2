module Blc.Queries.AreaResult
  ( AreaResult (AreaResult)
  , getAreaResult
  ) where

import Database.Esqueleto
import Data.Time
import Control.Exception (assert)
import Control.Monad.IO.Class (liftIO)
import Blc.Queries.DescendantBlcs
import Blc.Queries.Durations
import Area.Types
import Schema
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
getAreaResult start end aid =
  let d (k1, n1) (k2, n2) = assert (k1 == k2) (if n2 /= 0 then n1 / n2 else 1)
  in do
    mArea <- runDb (get aid)
    case mArea of
      Nothing -> return Nothing
      Just area -> do
        bids <- descendantBlcsOf aid
        uptimeDemands <- durations start end bids UptimeDemand
        demands <- durations start end bids Demand
        performUptimes <- durations start end bids PerformUptime
        uptimes <- durations start end bids Uptime
        let numComply = length $ filter (> 0.95) $
                        assert (length uptimeDemands == length demands) $
                        zipWith d uptimeDemands demands
        let numQuality = length $ filter (> 0.95) $
                         assert (length performUptimes == length uptimes) $
                         zipWith d performUptimes uptimes
        modeInterv <- getModeInterv start end bids
        mvInterv <- numEvents start end bids MvInterv
        spInterv <- numEvents start end bids SpInterv
        mvSat <- durations start end bids MvSat
        cvAffBySat <- durations start end bids CvAffBySat
        let totalDuration = diffUTCTime end start * realToFrac (length bids)
        let mvSat' = if totalDuration /= 0
                     then realToFrac $ sum (map snd mvSat) / totalDuration
                     else assert (sum (map snd mvSat) == 0) 0
        let cvAffBySat' =
              if totalDuration /= 0
              then realToFrac $ sum (map snd cvAffBySat) / totalDuration
              else assert (sum (map snd cvAffBySat) == 0) 0
        return $ Just $ AreaResult (Entity aid area)
          numComply numQuality (length bids)
          modeInterv mvInterv spInterv mvSat' cvAffBySat'

getModeInterv :: UTCTime -> UTCTime -> [Key Blc] -> AppM Int
getModeInterv start end bids = do
  today <- liftIO (relativeDay 0)
  modeInterv <- runDb $ select $ from $ \ i -> do
    let a = i ^. BlcIntervalEnd >. val start
            &&. i ^. BlcIntervalEnd <=. val end
    let b = i ^. BlcIntervalCategory ==. val Uptime
    let c = i ^. BlcIntervalBlc `in_` valList bids
    let d = i ^. BlcIntervalEnd !=. val today
    where_ (a &&. b &&. c &&. d)
    return $ count (i ^. BlcIntervalEnd)
  return (unValue $ head modeInterv)

numEvents :: UTCTime -> UTCTime -> [Key Blc] -> EventType -> AppM Int
numEvents start end bids eventType = do
  num <- runDb $ select $ from $ \ i -> do
    let a = i ^. BlcEventTime >=. val start &&. i ^. BlcEventTime <=. val end
    let b = i ^. BlcEventCategory ==. val eventType
    let c = i ^. BlcEventBlc `in_` valList bids
    where_ (a &&. b &&. c)
    return $ count (i ^. BlcEventTime)
  return (unValue $ head num)
