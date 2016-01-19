{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Area.Types where

import Database.Persist.TH
import Database.Persist.Postgresql
import Servant
import Control.Monad.Except
import Data.Time
import Data.Text (Text, unpack)
import Data.Text.Read (decimal)

share [ mkPersist sqlSettings,
        mkMigrate "migrateArea",
        mkDeleteCascade sqlSettings ]
  [persistLowerCase|
    Area
      name         String
      description  String
      parent       AreaId Maybe
      demandCond   String
    AreaInterval
      area         AreaId
      start        UTCTime
      end          UTCTime
  |]

instance FromFormUrlEncoded Area where
  fromFormUrlEncoded params = runExcept $ do
    name <- maybe (throwError "Missing name")
                  (return . unpack)
                  (lookup "name" params)
    mParent <- case lookup "parent" params of
      Nothing -> return Nothing
      Just p  -> either (throwError)
                        (return . Just . toSqlKey . fromIntegral . fst)
                        (decimal p :: Either String (Integer, Text))
    description <- maybe (throwError "Missing description")
                         (return . unpack)
                         (lookup "description" params)
    demandCond <- maybe (throwError "Missing demand condition")
                        (return . unpack)
                        (lookup "demandcond" params)
    return (Area name description mParent demandCond)
