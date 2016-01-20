{-# LANGUAGE TemplateHaskell #-}

module Blc.Enums where

import Database.Persist.TH

data Objective = Near | Above | Below deriving (Show, Read, Eq)
derivePersistField "Objective"
