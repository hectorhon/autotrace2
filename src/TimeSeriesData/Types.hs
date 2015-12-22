module TimeSeriesData.Types where

import Data.Time

data TSValue = Continuous Double
             | Discrete String
             deriving (Eq, Ord, Show)

data TSPoint = TSPoint { timeOf  :: NominalDiffTime
                       , valueOf :: TSValue }
               deriving Show

type TSSegment = (TSPoint, TSPoint)

type TSInterval = (NominalDiffTime, NominalDiffTime)
