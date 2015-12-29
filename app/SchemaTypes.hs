{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module SchemaTypes where

import Database.Persist.TH
import Data.Aeson
import GHC.Generics

data Objective = Near | Above | Below deriving (Show, Read, Eq)
derivePersistField "Objective"

data MetricType = Demand
                | UptimeDemand
                | Uptime
                | PerformUptime
                | MvSat
                | CvAffBySat deriving (Generic, Show, Read, Eq)
derivePersistField "MetricType"

instance ToJSON MetricType
instance FromJSON MetricType

data EventType = MvInterv | SpInterv deriving (Show, Read, Eq)
derivePersistField "EventType"
