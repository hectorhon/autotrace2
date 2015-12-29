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
import Data.Maybe (listToMaybe)
import Data.Text (Text, unpack, pack)
import Data.Text.Read (Reader, decimal, double)
import SchemaTypes
import Time

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
