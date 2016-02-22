{-# LANGUAGE TemplateHaskell #-}

module User.Enums where

import Database.Persist.TH

data RoleType = LopcUserRole
              | LopcAdminRole
              | ControlAdminRole
              | AreaAdminRole
              | ManageUsersRole
              | SysAdminRole
              deriving (Show, Read, Eq)
derivePersistField "RoleType"
