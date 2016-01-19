{-# LANGUAGE TemplateHaskell #-}

module Blc.Enums where

import Database.Persist.TH

data Objective = Near | Above | Below deriving (Show, Read, Eq)
derivePersistField "Objective"

data MetricType = Demand
                | UptimeDemand
                | Uptime
                | PerformUptime
                | MvSat
                | CvAffBySat
                deriving (Show, Read, Eq)
derivePersistField "MetricType"

data EventType = MvInterv | SpInterv deriving (Show, Read, Eq)
derivePersistField "EventType"

