{-# LANGUAGE TemplateHaskell #-}

module Lopc.Enums where

import Database.Persist.TH

data LopcClassification = MajorLopc
                        | MinorLopc
                        | OtherLopc deriving (Show, Read, Eq, Ord)
derivePersistField "LopcClassification"
