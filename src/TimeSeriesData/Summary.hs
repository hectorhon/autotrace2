module TimeSeriesData.Summary where

import TimeSeriesData.Types
import Time
import Control.Exception (assert)

-- Intervals must be sorted in ascending order. Result in seconds
groupByDay :: [Day] -> [TSInterval] -> [[TSInterval]]
groupByDay [] _ = []
groupByDay days [] = map (\ _ -> []) days
groupByDay (d:ds) intervals =
  (map trim a) : (groupByDay ds nextIntervals)
  where (a, b) = takeSection intervals
        takeSection = if not (isOfDay (addDays (-1) d) (head intervals))
                      then span (isOfDay d)
                      else (span (isOfDay d) . dropWhile (not . isOfDay d))
        trim (start, end) = (max start dayStart, min end dayEnd)
        nextIntervals = if null a then b
                        else let (start, end) = last a
                             in if start < dayEnd && dayEnd < end
                                then (last a) : b
                                else b
        dayStart = diffUTCTime (localDayToUTC d) refTime
        dayEnd   = diffUTCTime (localDayToUTC $ addDays 1 d) refTime

sumByDay :: [Day] -> [TSInterval] -> [Double]
sumByDay ds xs =
  assert (sum (map durationOf xs) == sum result) result
  where result = map (foldr (+) 0 . map durationOf) $ groupByDay ds xs
        durationOf (s, e) = realToFrac (e - s)

isOfDay :: Day -> TSInterval -> Bool
isOfDay day (s, e) = or [ start <= start' && end   >= end'
                        , start >= start' && start <= end'
                        , end   >= start' && start <= end' ]
  where start  = addUTCTime s refTime
        end    = addUTCTime e refTime
        start' = localDayToUTC day
        end'   = localDayToUTC (addDays 1 day)

groupByDay' :: [Day] -> [NominalDiffTime] -> [[NominalDiffTime]]
groupByDay' [] _ = []
groupByDay' (d:ds) events = a : groupByDay' ds b
  where (a, b) = (span (isOfDay' d) . dropWhile (not . isOfDay' d)) events

sumByDay' :: [Day] -> [NominalDiffTime] -> [Int]
sumByDay' ds xs = assert (length ds == sum result) result
  where result = map length (groupByDay' ds xs)

isOfDay' :: Day -> NominalDiffTime -> Bool
isOfDay' day t = start <= time && time < end
  where time  = addUTCTime t refTime
        start = localDayToUTC day
        end   = localDayToUTC (addDays 1 day)
