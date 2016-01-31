{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lopc.Types
  ( module Lopc.Types
  , module Lopc.Enums
  ) where

import Database.Persist.TH
import Servant
import Control.Monad.Except
import Data.Maybe (listToMaybe, isJust)
import Data.Text (Text, unpack, toLower)
import Data.Text.Read (double)
import Time
import Lopc.Enums

share [ mkPersist sqlSettings,
        mkMigrate "migrateLopc",
        mkDeleteCascade sqlSettings ]
  [persistLowerCase|
    Lopc
      reportedOn        Day
      area1             Text
      area2             Text
      area3             Text
      classification    LopcClassification
      framework         Text
      fluid             Text
      composition       Text
      pressure          Double Maybe
      hazardous         Bool
      equipmentTag      Text
      equipmentType     Text
      description       Text
      otherRef          Text
      remarks           Text
      closedOn          Day Maybe
  |]

instance FromFormUrlEncoded Lopc where
  fromFormUrlEncoded params = runExcept $ do
    reportedOn     <- lookup' "reportedOn" params (parseDay . unpack)
    area1          <- lookup' "area1" params return
    area2          <- lookup' "area2" params return
    area3          <- lookup' "area3" params return
    classification <- fmap fst $ lookup' "classification" params
                                         (listToMaybe . reads. unpack)
    framework      <- lookup' "framework" params return
    fluid          <- lookup' "fluid" params return
    composition    <- lookup' "composition" params return
    pressure       <- maybe (throwError "Missing pressure")
                            (either (\ _ -> return Nothing)
                                    (return . Just . fst) . double)
                            (lookup "pressure" params)
    hazardous      <- return (isJust (lookup "hazardous" params))
    equipmentTag   <- lookup' "equipmentTag" params return
    equipmentType  <- lookup' "equipmentType" params return
    description    <- lookup' "description" params return
    otherRef       <- lookup' "otherRef" params return
    remarks        <- lookup' "remarks" params return
    closedOn       <- maybe (throwError "Missing closedOn")
                            (return . parseDay . unpack)
                            (lookup "closedon" params)
    return (Lopc reportedOn area1 area2 area3 classification framework
                 fluid composition pressure hazardous equipmentTag equipmentType
                 description otherRef remarks closedOn)

lookup' :: Text -> [(Text, Text)] -> (Text -> Maybe a) -> Except String a
lookup' param params transform =
  maybe (throwError ("Missing or invalid " ++ (unpack param))) return
        (lookup (toLower param) params >>= transform)
