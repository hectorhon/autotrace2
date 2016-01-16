{-# LANGUAGE OverloadedStrings #-}
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
import Servant
import Data.Time
import Data.Text (unpack)
import Data.ByteString.Char8 (ByteString, pack)
import Control.Monad.Except
import User.Enums

share [ mkPersist sqlSettings,
        mkMigrate "migrateUser",
        mkDeleteCascade sqlSettings ]
  [persistLowerCase|
    User
      name          String
      hash          ByteString
    Role
      user          UserId
      role          RoleType
    Session
      ident         ByteString
      user          UserId
      loginOn       UTCTime
  |]

data LoginData = LoginData String ByteString

instance FromFormUrlEncoded LoginData where
  fromFormUrlEncoded params = runExcept $ do
    name <- maybe (throwError "Missing username")
                  (return . unpack)
                  (lookup "name" params)
    password <- maybe (throwError "Missing password")
                      (return . pack . unpack)
                      (lookup "password" params)
    return (LoginData name password)
