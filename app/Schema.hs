{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Schema
  ( module Schema
  , module SchemaTypes
  ) where

import Database.Persist.TH
import Database.Persist.Postgresql
import Servant
import Control.Monad.Except
import Data.Maybe (listToMaybe, isJust)
import Data.Text (Text, unpack, pack)
import Data.Text.Read (Reader, decimal, double)
import SchemaTypes
import Time

share [ mkPersist sqlSettings,
        mkMigrate "migrateAll",
        mkDeleteCascade sqlSettings ]
  [persistLowerCase|
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

instance ToBackendKey SqlBackend a => FromText (Key a) where
  fromText = either (\ _ -> Nothing) (Just . toSqlKey . fromIntegral . fst)
             . (decimal :: Reader Integer)

instance ToBackendKey SqlBackend a => ToText (Key a) where
  toText = pack . show . fromSqlKey

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

instance FromFormUrlEncoded Blc where
  fromFormUrlEncoded params = runExcept $ do
    name        <- maybe (throwError "Missing name")
                         (return . unpack)
                         (lookup "name" params)
    description <- maybe (throwError "Missing description")
                         (return . unpack)
                         (lookup "description" params)
    area        <- maybe (throwError "Missing or invalid area")
                         (return)
                         (lookup "area" params >>= fromText)
    measTag     <- maybe (throwError "Missing measurement tag")
                         (return . unpack)
                         (lookup "meastag" params)
    sptTag      <- maybe (throwError "Missing setpoint tag")
                         (return . unpack)
                         (lookup "spttag" params)
    outTag      <- maybe (throwError "Missing output tag")
                         (return . unpack)
                         (lookup "outtag" params)
    outMax      <- maybe (throwError "Missing output max")
                         (either (throwError . (++) "Output max ")
                                 (return . fst)
                          . double)
                         (lookup "outmax" params)
    outMin      <- maybe (throwError "Missing output min")
                         (either (throwError . (++) "Output min ")
                                 (return . fst)
                          . double)
                         (lookup "outmin" params)
    demandCond  <- maybe (throwError "Missing demand condition")
                         (return . unpack)
                         (lookup "demandcond" params)
    uptimeCond  <- maybe (throwError "Missing uptime condition")
                         (return . unpack)
                         (lookup "uptimecond" params)
    objective   <- maybe (throwError "Missing or invalid objective")
                         (return . fst)
                         (lookup "objective" params
                          >>= listToMaybe . reads . unpack)
    margin      <- maybe (throwError "Invalid or missing margin")
                         (either (throwError . (++) "Margin ")
                                 (return . fst)
                          . double)
                         (lookup "margin" params)
    calcMvICond <- maybe (throwError "Missing calculate MV interv. condition")
                         (return . unpack)
                         (lookup "calcmvicond" params)
    calcSpICond <- maybe (throwError "Missing calculate SP interv. condition")
                         (return . unpack)
                         (lookup "calcspicond" params)
    return $ Blc name description area measTag sptTag outTag outMax outMin
                 demandCond uptimeCond objective margin calcMvICond calcSpICond

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
