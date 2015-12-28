module TimeSeriesData.Interpolate where

import Data.Time

interpolate :: ((NominalDiffTime, Double), (NominalDiffTime, Double))
            -> NominalDiffTime
            -> Double
interpolate ((t1, y1), (t2, y2)) t =
  interpolate' ((realToFrac t1, y1), (realToFrac t2, y2)) (realToFrac t)

interpolate' :: ((Double, Double), (Double, Double)) -> Double -> Double
interpolate' ((x1, y1), (x2, y2)) x = (y2 - y1) / (x2 - x1) * (x - x1) + y1
