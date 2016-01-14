{-# LANGUAGE TemplateHaskell #-}

module User.Enums where

import Database.Persist.TH

data RoleType = ReadRole
              | WriteRole
              deriving (Show, Read, Eq)
derivePersistField "RoleType"
