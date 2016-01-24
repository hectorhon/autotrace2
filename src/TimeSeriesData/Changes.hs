module TimeSeriesData.Changes where

import TimeSeriesData.Types
import Data.Time

changesIn :: [TSPoint] -> [TSInterval] -> [NominalDiffTime]
changesIn points intervals = changesIn' Nothing points intervals

changesIn' :: Maybe TSValue -> [TSPoint] -> [TSInterval] -> [NominalDiffTime]
changesIn' previous (point:points) (interval:intervals) =
  if fst interval <= timeOf point && timeOf point <= snd interval
  then let rm = changesIn' (Just $ valueOf point) points (interval:intervals) in
       case previous of Nothing -> rm
                        Just v  -> if v == valueOf point then rm
                                   else timeOf point : rm
  else if timeOf point > snd interval
       then changesIn' Nothing (point:points) intervals
       else changesIn' Nothing points (interval:intervals)
changesIn' _ _ _ = []
