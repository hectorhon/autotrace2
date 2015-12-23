{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Schema
  ( module Schema
  , module SchemaTypes
  ) where

import Database.Persist.TH
import Database.Persist.Postgresql
import Data.Time
import SchemaTypes
import Servant
import Control.Monad.Except
import Data.Text (Text, unpack, pack)
import Data.Text.Read (Reader, decimal)

share [ mkPersist sqlSettings,
        mkMigrate "migrateAll",
        mkDeleteCascade sqlSettings ]
  [persistLowerCase|
    Apc
      name         String
      uptimeCond   String
    ApcUptime json
      apc          ApcId
      start        UTCTime
      end          UTCTime
    ApcIssue json
      apc          ApcId
      start        UTCTime
      end          UTCTime
      category     String
      description  String
      affectUptime Bool
    Cv json
      name         String
      apc          ApcId
      measTag      String
      srhTag       String
      srlTag       String
      predTag      String
      selTag       String
    CvInterval json
      cv           CvId
      start        UTCTime
      end          UTCTime
    Area
      name         String
      description  String
      parent       AreaId Maybe
    Blc
      name         String
      description  String
      area         AreaId
      measTag      String
      sptTag       String
      outTag       String
      outMax       Double
      outMin       Double
      demandCond   String
      uptimeCond   String
      objective    Objective
      margin       Double
      calcMvICond  String
      calcSpICond  String
    BlcInterval
      blc          BlcId
      start        UTCTime
      end          UTCTime
      category     MetricType
    BlcEvent
      blc          BlcId
      time         UTCTime
      category     EventType
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
    return (Area name description mParent)

instance FromText (Key Area) where
  fromText = either (\ _ -> Nothing) (Just . toSqlKey . fromIntegral . fst)
             . (decimal :: Reader Integer)

instance ToText (Key Area) where
  toText = pack . show . fromSqlKey
