module TimeSeriesData.Arithmetic where

import Control.Exception (assert)
import TimeSeriesData.Types
import TimeSeriesData.Interpolate

arithmetic :: (Double -> Double -> Double) -> [TSPoint] -> [TSPoint]
           -> [TSPoint]
arithmetic operation reds blacks = zipWith operation' reds' blacks'
  where (reds', blacks') = sync reds blacks
        operation' (TSPoint t1 (Continuous y1)) (TSPoint _ (Continuous y2)) =
          TSPoint t1 (Continuous (operation y1 y2))
        -- If either not continuous, operation invalid, so return the discrete
        operation' (TSPoint t1 (Discrete s1)) _ = TSPoint t1 (Discrete s1)
        operation' _ (TSPoint t1 (Discrete s1)) = TSPoint t1 (Discrete s1)

sync :: [TSPoint] -> [TSPoint] -> ([TSPoint], [TSPoint])
sync (red:reds) (black:blacks) = assert (timeOf red == timeOf black) $
  let (reds', blacks') = sync' red black reds blacks
  in (red:reds', black:blacks')
sync _ _ = ([], [])

sync' :: TSPoint -> TSPoint -> [TSPoint] -> [TSPoint] -> ([TSPoint], [TSPoint])
sync' lastRed lastBlack (red:reds) (black:blacks)
  | timeOf red == timeOf black =
      let (reds', blacks') = sync' red black reds blacks
      in (red:reds', black:blacks')
  | timeOf red < timeOf black =
      case (valueOf lastBlack, valueOf black) of
        (Continuous y1, Continuous y2) ->
          let black' = TSPoint (timeOf red)
                               (Continuous $
                                interpolate
                                ((timeOf lastBlack, y1), (timeOf black, y2))
                                (timeOf red))
              (reds', blacks') = sync' red black' reds (black:blacks)
          in (red:reds', black':blacks')
        (v1, _) ->
          let black' = TSPoint (timeOf red) v1
              (reds', blacks') = sync' red black' reds (black:blacks)
          in (red:reds', black':blacks')
  | timeOf red > timeOf black =
      case (valueOf lastRed, valueOf red) of
        (Continuous y1, Continuous y2) ->
          let red' = TSPoint (timeOf black)
                               (Continuous $
                                interpolate
                                ((timeOf lastRed, y1), (timeOf red, y2))
                                (timeOf black))
              (reds', blacks') = sync' red' black (red:reds) blacks
          in (red':reds', black:blacks')
        (v1, _) ->
          let red' = TSPoint (timeOf black) v1
              (reds', blacks') = sync' red' black (red:reds) blacks
          in (red':reds', black:blacks')
sync' _ _ _ _ = ([], [])
