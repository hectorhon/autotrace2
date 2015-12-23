{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Schema
  ( module Schema
  , module SchemaTypes
  ) where

import Database.Persist.TH
import Data.Time
import SchemaTypes

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
