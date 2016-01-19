{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module SchemaTypes where

import Database.Persist.TH
import Data.Aeson
import GHC.Generics

data MetricType = Uptime
                | Exceed
                deriving (Generic, Show, Read, Eq)
derivePersistField "MetricType"

instance ToJSON MetricType
instance FromJSON MetricType

data EventType = MvInterv | SpInterv deriving (Show, Read, Eq)
derivePersistField "EventType"

data CvType = EconomicCv
            | ConstraintCv
            | ProtectiveCv
            deriving (Generic, Show, Read, Eq)
derivePersistField "CvType"

instance ToJSON CvType
instance FromJSON CvType
