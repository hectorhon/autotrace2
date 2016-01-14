{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module User.Types
  ( module User.Types
  , module User.Enums
  ) where

import Database.Persist.TH
import Data.Time
import Data.Text
import Data.ByteString
import User.Enums

share [ mkPersist sqlSettings,
        mkMigrate "migrateUser",
        mkDeleteCascade sqlSettings ]
  [persistLowerCase|
    User
      name          Text
      hash          ByteString
    Role
      user          UserId
      role          RoleType
    Session
      ident         ByteString
      user          UserId
      loginOn       UTCTime
  |]
