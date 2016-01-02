{-# LANGUAGE FlexibleContexts #-}

module Blc.Queries.BlcResult 
  ( BlcResult (BlcResult)
  , getChildBlcResult
  , getCompliances
  , getQualities
  ) where

import Database.Esqueleto
import Data.Time
import Control.Exception (assert)
import Data.IntMap (fromAscList, fromList, union, toAscList)
import Blc.Queries.Durations
import AppM
import Schema

data BlcResult =
  BlcResult (Entity Blc)
            Double        -- ^ Compliance
            Double        -- ^ Quality
            Int           -- ^ Mode intervention
            Int           -- ^ MV intervention
            Int           -- ^ SP intervention
            Double        -- ^ MV saturation
            Double        -- ^ CV affected by saturation

getChildBlcResult :: UTCTime -> UTCTime -> Key Area -> AppM [BlcResult]
getChildBlcResult start end aid =
  let totalDuration = diffUTCTime end start
  in do
    blcs <- runDb $ select $ from $ \ i -> do
      where_ (i ^. BlcArea ==. val aid)
      return i
    let bids = map entityKey blcs
    compliances <- getCompliances start end bids
    qualities <- getQualities start end bids
    modeInterv <- getModeInterv start end bids
    mvInterv <- numEvents start end bids MvInterv
    spInterv <- numEvents start end bids SpInterv
    mvSat <- durations start end bids MvSat
    cvAffBySat <- durations start end bids CvAffBySat
    let dv (k1, n1) n = (k1, if n /= 0 then realToFrac (n1/n) else 1)
    let mvSat' = zipWith dv mvSat (repeat totalDuration)
    let cvAffBySat' = zipWith dv cvAffBySat (repeat totalDuration)
    assert
      (and $ zipWith8' (\ _ b c d e f g h -> and $ map (== b) [c,d,e,f,g,h])
                       ([undefined]) (map fst compliances) (map fst qualities)
                       (map fst modeInterv) (map fst mvInterv)
                       (map fst spInterv)
                       (map fst mvSat') (map fst cvAffBySat'))
      (return $ zipWith8' BlcResult blcs
                                    (map snd compliances)
                                    (map snd qualities)
                                    (map snd modeInterv)
                                    (map snd mvInterv)
                                    (map snd spInterv)
                                    (map snd mvSat')
                                    (map snd cvAffBySat'))

getCompliances :: UTCTime -> UTCTime -> [Key Blc] -> AppM [(Key Blc, Double)]
getCompliances start end bids = do
  uptimeDemands <- durations start end bids UptimeDemand
  demands <- durations start end bids Demand
  let d (k1, n1) (k2, n2) = assert (k1 == k2) $
                            if n2 /= 0 then (k1, realToFrac $ n1/n2) else (k1,1)
  let compliances = assert (length uptimeDemands == length demands) $
                    zipWith d uptimeDemands demands
  return compliances

getQualities :: UTCTime -> UTCTime -> [Key Blc] -> AppM [(Key Blc, Double)]
getQualities start end bids = do
  performUptimes <- durations start end bids PerformUptime
  uptimes <- durations start end bids Uptime
  let d (k1, n1) (k2, n2) = assert (k1 == k2) $
                            if n2 /= 0 then (k1, realToFrac $ n1/n2) else (k1,1)
  let qualities = assert (length performUptimes == length uptimes) $
                  zipWith d performUptimes uptimes
  return qualities

getModeInterv :: UTCTime -> UTCTime -> [Key Blc] -> AppM [(Key Blc, Int)]
getModeInterv start end bids = do
  modeInterv <- runDb $ select $ from $ \ i -> do
    let a = i ^. BlcIntervalEnd >. val start
            &&. i ^. BlcIntervalEnd <=. val end
    let b = i ^. BlcIntervalCategory ==. val Uptime
    let c = i ^. BlcIntervalBlc `in_` valList bids
    where_ (a &&. b &&. c)
    groupBy (i ^. BlcIntervalBlc)
    orderBy [asc (i ^. BlcIntervalBlc)]
    return (i ^. BlcIntervalBlc, count (i ^. BlcIntervalEnd))
  let encode (Value k, Value d) = (fromIntegral (fromSqlKey k), d)
  let modeInterv' = fromAscList (map encode modeInterv)
  let def = fromList $ zip (map (fromIntegral . fromSqlKey) bids) (repeat 0)
  let result = union modeInterv' def
  return $ map (\ (k, d) -> (toSqlKey (fromIntegral k), d)) (toAscList result)

numEvents :: UTCTime -> UTCTime -> [Key Blc] -> EventType
           -> AppM [(Key Blc, Int)]
numEvents start end bids eventType = do
  nums <- runDb $ select $ from $ \ i -> do
    let a  = i ^. BlcEventTime >. val start &&. i ^. BlcEventTime <=. val end
    let b  = i ^. BlcEventCategory ==. val eventType
    let c  = i ^. BlcEventBlc `in_` valList bids
    where_ (a &&. b &&. c)
    groupBy (i ^. BlcEventBlc)
    orderBy [asc (i ^. BlcEventBlc)]
    return (i ^. BlcEventBlc, count (i ^. BlcEventTime))
  let encode (Value k, Value d) = (fromIntegral (fromSqlKey k), d)
  let nums' = fromAscList (map encode nums)
  let def = fromList $ zip (map (fromIntegral . fromSqlKey) bids) (repeat 0)
  let result = union nums' def
  return $ map (\ (k, d) -> (toSqlKey (fromIntegral k), d)) (toAscList result)

zipWith8' :: (a -> b -> c -> d -> e -> f -> g -> h -> z)
          -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [h] -> [z]
zipWith8' fx c1 c2 c3 c4 c5 c6 c7 c8 = assert
  (length c1 == length c2 && length c2 == length c4 && length c3 == length c4
   && length c4 == length c5 && length c5 == length c6 && length c6 == length c7
   && length c7 == length c8)
  (zipWith8 fx c1 c2 c3 c4 c5 c6 c7 c8)

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
