{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Blc.Types
  ( module Blc.Types
  , module Blc.Enums
  ) where

import Database.Persist.TH
import Servant
import Control.Monad.Except
import Data.Maybe
import Data.Time
import Data.Text (unpack)
import Data.Text.Read (double)
import PersistKeyInstances ()
import Area.Types
import Blc.Enums

share [ mkPersist sqlSettings,
        mkMigrate "migrateBlc",
        mkDeleteCascade sqlSettings ]
  [persistLowerCase|
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
    BlcResultData
      blc           BlcId
      day           Day
      demand        Double   -- All stats in seconds
      uptimeDemand  Double
      uptime        Double
      performUptime Double
      modeInterv    Int
      mvInterv      Int
      spInterv      Int
      mvSat         Double
      cvAffBySat    Double
  |]

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
