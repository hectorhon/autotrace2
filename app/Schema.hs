{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Schema
  ( module Schema
  , module SchemaTypes
  ) where

import Database.Persist.TH
import Servant
import Control.Monad.Except
import Data.Maybe (listToMaybe, isJust)
import Data.Text (unpack)
import Area.Types
import SchemaTypes
import Time
import PersistKeyInstances ()

share [ mkPersist sqlSettings,
        mkMigrate "migrateAll",
        mkDeleteCascade sqlSettings ]
  [persistLowerCase|
    Apc
      name         String
      area         AreaId
      uptimeCond   String
    ApcInterval json
      apc          ApcId
      start        UTCTime
      end          UTCTime
      category     MetricType
    ApcIssue json
      apc            ApcId
      start          UTCTime
      end            UTCTime
      category       String
      description    String
      discountUptime Bool
    Cv json
      name         String
      apc          ApcId
      category     CvType
      measTag      String
      srhTag       String
      srlTag       String
      predTag      String
      selTag       String
    CvInterval json
      cv           CvId
      start        UTCTime
      end          UTCTime
      category     MetricType
    Mv json
      name         String
      apc          ApcId
      measTag      String
      srhTag       String
      srlTag       String
      predTag      String
      selTag       String
    MvInterval json
      mv           MvId
      start        UTCTime
      end          UTCTime
      category     MetricType
    BlockHead
      name         String
      type_        String
      firstDate    UTCTime
      currentDate  UTCTime
      group        String
      NameTypeGroup name type_ group
    BlockAttr
      block        BlockHeadId
      changedOn    UTCTime
      key          String
      value        String
  |]

instance FromFormUrlEncoded Apc where
  fromFormUrlEncoded params = runExcept $ do
    name <- maybe (throwError "Missing name")
                  (return . unpack)
                  (lookup "name" params)
    area <- maybe (throwError "Missing or invalid area")
                  (return)
                  (lookup "area" params >>= fromText)
    uptimeCond <- maybe (throwError "Missing uptime condition")
                        (return . unpack)
                        (lookup "uptimecond" params)
    return (Apc name area uptimeCond)

instance FromFormUrlEncoded Cv where
  fromFormUrlEncoded params = runExcept $ do
    name    <- maybe (throwError "Missing name")
                     (return . unpack)
                     (lookup "name" params)
    apc     <- maybe (throwError "Missing or invalid apc")
                     (return)
                     (lookup "area" params >>= fromText)
    category <- maybe (throwError "Missing or invalid category")
                      (return . fst)
                      (lookup "category" params
                       >>= listToMaybe . reads . unpack)
    measTag <- maybe (throwError "Missing measurement tag")
                     (return . unpack)
                     (lookup "meastag" params)
    srhTag  <- maybe (throwError "Missing set range high tag")
                     (return . unpack)
                     (lookup "srhtag" params)
    srlTag  <- maybe (throwError "Missing set range low tag")
                     (return . unpack)
                     (lookup "srltag" params)
    predTag <- maybe (throwError "Missing prediction tag")
                     (return . unpack)
                     (lookup "predtag" params)
    selTag  <- maybe (throwError "Missing selected tag")
                     (return . unpack)
                     (lookup "seltag" params)
    return (Cv name apc category measTag srhTag srlTag predTag selTag)

instance FromFormUrlEncoded ApcIssue where
  fromFormUrlEncoded params = runExcept $ do
    apcId          <- maybe (throwError "Missing or invalid apc")
                            (return)
                            (lookup "apc" params >>= fromText)
    startDay       <- maybe (throwError "Missing start day")
                            (maybe (throwError "Failed to parse start day")
                                   return
                             . parseDay . unpack)
                            (lookup "startday" params)
    startTime      <- maybe (throwError "Missing start time")
                            (maybe (throwError "Failed to parse start time")
                                   return
                             . parseClock . unpack)
                            (lookup "starttime" params)
    endDay         <- maybe (throwError "Missing end day")
                              (maybe (throwError "Failed to parse end day")
                                     return
                               . parseDay . unpack)
                              (lookup "endday" params)
    endTime        <- maybe (throwError "Missing end time")
                            (maybe (throwError "Failed to parse end time")
                                   return
                             . parseClock . unpack)
                            (lookup "endtime" params)
    category       <- maybe (throwError "Missing category")
                            (return . unpack)
                            (lookup "category" params)
    description    <- maybe (throwError "Missing description")
                            (return . unpack)
                            (lookup "description" params)
    discountUptime <- return $ isJust (lookup "discountuptime" params)
    let start      =  localTimeToUTC tz (LocalTime startDay startTime)
    let end        =  localTimeToUTC tz (LocalTime endDay endTime)
    return (ApcIssue apcId start end category description discountUptime)
