{-# LANGUAGE OverloadedStrings #-}

module TimeSeriesData.Types where

import Data.Time
import Data.ByteString
import Data.Attoparsec.ByteString.Char8

data TSValue = Continuous Double
             | Discrete String
             deriving (Eq, Ord, Show)

data TSPoint = TSPoint { timeOf  :: NominalDiffTime
                       , valueOf :: TSValue }
               deriving Show

type TSSegment = (TSPoint, TSPoint)

type TSInterval = (NominalDiffTime, NominalDiffTime)

data TSData = TSData { startOf :: NominalDiffTime
                     , endOf   :: NominalDiffTime
                     , dataOf  :: [(String, [TSPoint])] }

decode :: ByteString -> Maybe [TSPoint]
decode raw = maybeResult $ feed (parse tsPointsParser raw) "\n"
  where tsPointsParser = (many' $ tsPointParser <* endOfLine)
        tsPointParser = do t <- decimal
                           _ <- char '\t'
                           v <- tsValueParser
                           return $ TSPoint (fromIntegral (t :: Integer)) v
        tsValueParser = choice [continuousParser, discreteParser]
        continuousParser = double >>= return . Continuous
        discreteParser = do _ <- char '"'
                            v <- many' $ notChar '"'
                            _ <- char '"'
                            return $ Discrete v
