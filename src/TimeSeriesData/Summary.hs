module TimeSeriesData.Summary where

import TimeSeriesData.Types
import Time
import Control.Exception (assert)

-- Intervals must be sorted in ascending order. Result in seconds
groupByDay :: [Day] -> [TSInterval] -> [[TSInterval]]
groupByDay [] _ = []
groupByDay days [] = replicate (length days) []
groupByDay (d:ds) intervals = (map trim a) : groupByDay ds nextIntervals
  where (a, b) = (span isNotAfterDay . dropWhile isBeforeDay) intervals
        isBeforeDay   (_, end)   = end <= dayStart
        isNotAfterDay (start, _) = start < dayEnd
        trim (start, end) = (max start dayStart, min end dayEnd)
        nextIntervals = if null a then b
                        else let (start, end) = last a
                             in if start < dayEnd && dayEnd < end
                                then (last a) : b
                                else b
        dayStart      = dayToDiffTime d
        dayEnd        = dayToDiffTime (addDays 1 d)
        dayToDiffTime = (flip diffUTCTime) refTime . localDayToUTC

sumByDay :: [Day] -> [TSInterval] -> [Double]
sumByDay ds xs =
  assert (sum (map durationOf xs) == sum result) result
  where result = map (foldr (+) 0 . map durationOf) $ groupByDay ds xs
        durationOf (s, e) = realToFrac (e - s)

groupByDay' :: [Day] -> [NominalDiffTime] -> [[NominalDiffTime]]
groupByDay' [] _ = []
groupByDay' (d:ds) events = a : groupByDay' ds b
  where (a, b) = (span isNotAfterDay . dropWhile isBeforeDay) events
        isBeforeDay   t = t <= dayStart
        isNotAfterDay t = t <= dayEnd
        dayStart         = dayToDiffTime d
        dayEnd           = dayToDiffTime (addDays 1 d)
        dayToDiffTime    = (flip diffUTCTime) refTime . localDayToUTC

sumByDay' :: [Day] -> [NominalDiffTime] -> [Int]
sumByDay' ds xs = assert (length xs == sum result) result
  where result = map length (groupByDay' ds xs)
