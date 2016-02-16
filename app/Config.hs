module Config where

import Database.Persist.Postgresql
import Control.Concurrent
import Data.ByteString
import Job.Types

data Config = Config { getPool :: ConnectionPool
                     , getPoolConnStr :: ByteString
                     , getQSemN :: QSemN
                     , getCounter :: MVar Int
                     , getSrcUrl :: String
                     , getSrcPort :: Int
                     , getMaxQSemN :: Int
                     , getChan :: Chan JobQueueItem
                     }

defaultConfig :: Config
defaultConfig = Config { getPool = undefined
                       , getPoolConnStr = undefined
                       , getQSemN = undefined
                       , getCounter = undefined
                       , getSrcUrl = undefined
                       , getSrcPort = undefined
                       , getMaxQSemN = undefined
                       , getChan = undefined
                       }
