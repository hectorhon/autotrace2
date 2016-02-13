{-# LANGUAGE OverloadedStrings #-}

module TimeSeriesData.Types where

import Data.Time
import Data.Text
import Data.Attoparsec.Text
import Data.Aeson
import Control.Applicative

data TSValue = Continuous Double
             | Discrete String
             deriving (Eq, Ord, Show)

data TSPoint = TSPoint { timeOf  :: NominalDiffTime
                       , valueOf :: TSValue }
               deriving Show

instance ToJSON TSPoint where
  toJSON (TSPoint t (Continuous y)) = object [ "t" .= (realToFrac t :: Double)
                                             , "v" .= y ]
  toJSON (TSPoint t (Discrete   s)) = object [ "t" .= (realToFrac t :: Double)
                                             , "v" .= s ]

type TSSegment = (TSPoint, TSPoint)

type TSInterval = (NominalDiffTime, NominalDiffTime)

data TSData = TSData { startOf :: NominalDiffTime
                     , endOf   :: NominalDiffTime
                     , dataOf  :: [(String, [TSPoint])] }

decode :: Text -> Maybe [TSPoint]
decode raw = maybeResult (parse tsPointsParser raw)
  where tsPointsParser = many tsPointParser
        tsPointParser = do t <- decimal
                           _ <- char '\t'
                           v <- tsValueParser
                           _ <- endOfLine
                           return $ TSPoint (fromIntegral (t :: Integer)) v
        tsValueParser = continuousParser <|> discreteParser
        continuousParser = double >>= return . Continuous
        discreteParser = do _ <- char '"'
                            v <- many (notChar '"')
                            _ <- char '"'
                            return $ Discrete v
