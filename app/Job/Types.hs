{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Job.Types where

import Database.Persist.TH
import Database.Persist.Postgresql
import Data.Time
import Control.Monad.Reader
import Control.Concurrent
import User.Types

share [ mkPersist sqlSettings,
        mkMigrate "migrateJob",
        mkDeleteCascade sqlSettings ]
  [persistLowerCase|
    JobRecord
      description   String
      scheduledBy   UserId
      scheduledOn   UTCTime
      cancelled     Bool
      progress      Double
  |]

type WorkEnv = (String, Int, SqlBackend)
type Progress = (Int, Int)
type Work = ReaderT (WorkEnv, MVar Progress) IO ()
data JobQueueItem = JobQueueItem (Key JobRecord) Work
