module TimeSeriesData.Compares
  ( compares
  ) where

import Control.Exception (assert)
import TimeSeriesData.Types
import TimeSeriesData.Interpolate

compares :: [TSPoint] -> [TSPoint] -> [(TSInterval, Maybe Ordering)]
compares reds blacks = compress $
  compares' (toSegments reds) (toSegments blacks)
  where toSegments [] = []
        toSegments ps = zip (init ps) (tail ps)

compares' :: [TSSegment] -> [TSSegment] -> [(TSInterval, Maybe Ordering)]

compares' [] [] = []

compares' reds [] = [(interval, Nothing)]
  where start    = timeOf $ fst (head reds)
        end      = timeOf $ fst (last reds)
        interval = (start, end)

compares' [] blacks = [(interval, Nothing)]
  where start    = timeOf $ fst (head blacks)
        end      = timeOf $ fst (last blacks)
        interval = (start, end)

compares' reds blacks = assert (near t1 t3) $
  case (v1, v2, v3, v4) of
    (Continuous y1, Continuous y2, Continuous y3, Continuous y4) ->
      let mIntersect = intersection
                          ((realToFrac t1, y1), (realToFrac t2, y2))
                          ((realToFrac t3, y3), (realToFrac t4, y4))
                       >>= \ (t, y) -> return (realToFrac t, y)
          result = case mIntersect of
                     Nothing      -> if near y1 y3
                                     then ((t1, min t2 t4),
                                           Just (compare y2 y4))
                                     else ((t1, min t2 t4),
                                           Just (compare y1 y3))
                     Just (ti, _) -> ((t1, ti), Just (compare y1 y3))
          reds' = case mIntersect of
                    Nothing       -> let y = interpolate
                                               ((realToFrac t1, y1),
                                                (realToFrac t2, y2))
                                               (realToFrac t4) in
                                     if t2 > t4
                                     then (TSPoint t4 (Continuous y),
                                           TSPoint t2 (Continuous y2))
                                          : (tail reds)
                                     else tail reds
                    Just (ti, yi) -> (TSPoint ti (Continuous yi),
                                      TSPoint t2 (Continuous y2))
                                     : (tail reds)
          blacks' = case mIntersect of
                    Nothing       -> let y = interpolate
                                               ((realToFrac t3, y3),
                                                (realToFrac t4, y4))
                                               (realToFrac t2) in
                                     if t4 > t2
                                     then (TSPoint t2 (Continuous y),
                                           TSPoint t4 (Continuous y4))
                                          : (tail blacks)
                                     else tail blacks
                    Just (ti, yi) -> (TSPoint ti (Continuous yi),
                                      TSPoint t4 (Continuous y4))
                                     : (tail blacks)
      in result : (compares' reds' blacks')
    (Discrete s1, Discrete s2, Discrete s3, Discrete s4) ->
      let interval = (t1, min t2 t4)
          order    = Just (compare s1 s3)
          reds'    = if t2 > t4
                     then (TSPoint t4 (Discrete s1), TSPoint t2 (Discrete s2))
                          : (tail reds)
                     else tail reds
          blacks'  = if t4 > t2
                     then (TSPoint t2 (Discrete s3), TSPoint t4 (Discrete s4))
                          : (tail blacks)
                     else tail blacks
      in (interval, order) : (compares' reds' blacks')
    _ ->
      let interval = (t1, min t2 t4)
          reds'    = if t2 > t4
                     then (TSPoint t4 v1, TSPoint t2 v2) : (tail reds)
                     else tail reds
          blacks'  = if t4 > t2
                     then (TSPoint t2 v3, TSPoint t4 v4) : (tail blacks)
                     else tail blacks
      in (interval, Nothing) : (compares' reds' blacks')
  where (TSPoint t1 v1, TSPoint t2 v2) = head reds
        (TSPoint t3 v3, TSPoint t4 v4) = head blacks

intersection :: ((Double, Double), (Double, Double))
             -> ((Double, Double), (Double, Double))
             -> Maybe (Double, Double)
intersection ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4))
  | or [near d 0, s < 0, s > 1, t < 0, t > 1] = Nothing
  | near x1 x3 && near y1 y3 = Nothing
  | near x2 x4 && near y2 y4 = Nothing
  | otherwise = Just (x, y)
  where n = x1*(y2-y3) - x2*(y1-y3) + x3*(y1-y2)
        d = (x1-x2)*(y4-y3) + (x3-x4)*(y1-y2)
        t = n/d
        s = ((1-t)*x3 + t*x4 - x1) / (x2-x1)
        x = (x2-x1)*s + x1
        y = (y2-y1)*s + y1

compress :: [(TSInterval, Maybe Ordering)] -> [(TSInterval, Maybe Ordering)]
compress [] = []
compress (r:[]) = [r]
compress (r:rs) = compress' r rs

compress' :: (TSInterval, Maybe Ordering) -> [(TSInterval, Maybe Ordering)]
          -> [(TSInterval, Maybe Ordering)]
compress' r'@((start', end'), o') (r@((start, end), o):[])
  | (near end' start && o' == o) = [((start', end), o)]
  | otherwise                  = [r', r]
compress' r'@((start', end'), o') (r@((start, end), o):rs)
  | (near end' start && o' == o) = compress' ((start', end), o) rs
  | otherwise                  = r' : compress' r rs
compress' _ _ = assert False undefined

near :: RealFrac a => a -> a -> Bool
near a b = if rel == 0 then True else (abs a - abs b) / rel < 0.001 
  where rel = max (abs a) (abs b)
