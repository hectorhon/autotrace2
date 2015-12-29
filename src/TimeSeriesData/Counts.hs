module TimeSeriesData.Counts where

import Data.Time (NominalDiffTime)
import Data.List (sort)
import Control.Exception (assert)
import TimeSeriesData.Types (TSInterval)

data StartEnd = Start | End deriving (Eq, Ord)

counts :: [[TSInterval]] -> [(TSInterval, Int)]
counts ss = if null times then [] else counts' 0 (fst $ head times) times
  where toTimes = concat . map (\ (a, b) -> [(a, Start), (b, End)])
        times   = sort (concat $ map toTimes ss)

counts' :: Int -> NominalDiffTime -> [(NominalDiffTime, StartEnd)]
        -> [(TSInterval, Int)]
counts' acc time ((now, pos):[]) = assert (acc == 1 && pos == End) $
  if now == time then [] else [((time, now), acc)]
counts' acc time ((now, pos):times)
  | now == time = counts' (if pos == Start then acc + 1 else acc - 1) now times
  | now > time = ((time, now), acc)
                 : counts' (if pos == Start then acc + 1 else acc - 1) now times
counts' _ _ _ = assert False undefined
