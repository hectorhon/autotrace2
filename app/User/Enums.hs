{-# LANGUAGE TemplateHaskell #-}

module User.Enums where

import Database.Persist.TH

data RoleType = LopcUserRole
              | LopcAdminRole
              | ControlAdminRole
              | AreaAdminRole
              deriving (Show, Read, Eq)
derivePersistField "RoleType"
