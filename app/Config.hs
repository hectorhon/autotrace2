module Config where

import Database.Persist.Postgresql
import Control.Concurrent
import Data.ByteString

data Config = Config { getPool :: ConnectionPool
                     , getPoolConnStr :: ByteString
                     , getQSem :: QSem
                     , getCounter :: MVar Int
                     , getSrcUrl :: String
                     , getSrcPort :: Int
                     }

defaultConfig :: Config
defaultConfig = Config { getPool = undefined
                       , getPoolConnStr = undefined
                       , getQSem = undefined
                       , getCounter = undefined
                       , getSrcUrl = undefined
                       , getSrcPort = undefined
                       }
