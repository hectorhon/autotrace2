{-# LANGUAGE FlexibleContexts #-}

module Blc.Queries.Durations where

import Database.Esqueleto
import Data.Time
import Data.IntMap (fromAscList, fromList, unionWith, union, toAscList)
import Schema
import AppM

durations :: UTCTime -> UTCTime -> [Key Blc] -> MetricType
          -> AppM [(Key Blc, NominalDiffTime)]
durations start end bids metricType =
  let encode (Value k, Value d) = (fromIntegral (fromSqlKey k), maybe 0 id d)
  in do
    within <- runDb $ select $ from $ \ i -> do
      let a1 = i ^. BlcIntervalStart >=. val start
      let a2 = i ^. BlcIntervalEnd <=. val end
      let a  = a1 &&. a2
      let b  = i ^. BlcIntervalCategory ==. val metricType
      let c  = i ^. BlcIntervalBlc `in_` valList bids
      where_ (a &&. b &&. c)
      groupBy (i ^. BlcIntervalBlc)
      orderBy [asc (i ^. BlcIntervalBlc)]
      let duration = i ^. BlcIntervalEnd -. i ^. BlcIntervalStart
      return (i ^. BlcIntervalBlc, sum_ duration)
    let within' = fromAscList (map encode within)
    outside <- runDb $ select $ from $ \ i -> do
      let a1 = i ^. BlcIntervalStart >=. val start
               &&. i ^. BlcIntervalStart <. val end
               &&. i ^. BlcIntervalEnd >. val end
      let a2 = i ^. BlcIntervalEnd >. val start
               &&. i ^. BlcIntervalEnd <=. val end
               &&. i ^. BlcIntervalStart <. val start
      let a3 = i ^. BlcIntervalStart <. val start
               &&. i ^. BlcIntervalEnd >. val end
      let a  = a1 ||. a2 ||. a3
      let b  = i ^. BlcIntervalCategory ==. val metricType
      let c  = i ^. BlcIntervalBlc `in_` valList bids
      where_ (a &&. b &&. c)
      groupBy (i ^. BlcIntervalBlc)
      orderBy [asc (i ^. BlcIntervalBlc)]
      let s = case_ [
                when_ (i ^. BlcIntervalStart <. val start) then_ (val start)
                ] $ else_ (i ^. BlcIntervalStart)
      let e = case_ [
                when_ (i ^. BlcIntervalEnd >. val end) then_ (val end)
                ] $ else_ (i ^. BlcIntervalEnd)
      return (i ^. BlcIntervalBlc, sum_ (e -. s))
    let outside' = fromAscList (map encode outside)
    let def = fromList $ zip (map (fromIntegral . fromSqlKey) bids) (repeat 0)
    let d a b = if b /= 0 then a / b else 1
    let result = union (unionWith d within' outside') def
    return $ map (\ (k, v) -> (toSqlKey (fromIntegral k), v)) (toAscList result)
