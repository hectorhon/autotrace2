{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lopc.Types where

import Database.Persist.TH
import Data.Text
import Data.Time

share [ mkPersist sqlSettings,
        mkMigrate "migrateLopc",
        mkDeleteCascade sqlSettings ]
  [persistLowerCase|
    Lopc
      reportedOn        Day
      area1             Text
      area2             Text
      area3             Text
      classification    Text
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
