module Job.ScheduleJob
  ( scheduleJob
  , worker
  , monitor
  ) where

import Database.Persist.Postgresql
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Concurrent
import Job.Types
import User.Types
import Data.Time
import AppM
import Config

scheduleJob :: String -> Key User -> Work -> AppM ()
scheduleJob description uid work = do
  now <- liftIO getCurrentTime
  jobRecordKey <- runDb (insert (JobRecord description uid now 0))
  chan <- reader getChan
  liftIO (writeChan chan (JobQueueItem jobRecordKey work))

worker :: String -> Int -> ConnectionString -> Chan JobQueueItem -> IO ()
worker source port connString chan = runNoLoggingT $
  withPostgresqlConn connString $ \ conn -> lift $ forever $
    do JobQueueItem jobRecordKey work <- readChan chan
       progress <- newMVar undefined
       runNoLoggingT $ withPostgresqlConn connString $ \ conn' -> lift $ do
         tid <- forkIO (monitor progress jobRecordKey conn')
         runReaderT work ((source, port, conn), progress)
         killThread tid
       runSqlConn (update jobRecordKey [JobRecordProgress =. 100]) conn

monitor :: MVar (Int, Int) -> Key JobRecord -> SqlBackend -> IO ()
monitor progress jobRecordKey conn = forever $ do
  threadDelay (3 * 1000000)
  (completed, total) <- readMVar progress
  let progress' | total /= 0 = (realToFrac completed) / (realToFrac total)
                | otherwise = 0
  runSqlConn (update jobRecordKey [JobRecordProgress =. progress']) conn
