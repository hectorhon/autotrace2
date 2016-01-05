module Config where

import Database.Persist.Postgresql
import Control.Concurrent
import Data.ByteString

data Config = Config { getPool :: ConnectionPool
                     , getPoolConnStr :: ByteString
                     , getQSemN :: QSemN
                     , getCounter :: MVar Int
                     , getSrcUrl :: String
                     , getSrcPort :: Int
                     , getMaxQSemN :: Int
                     }

defaultConfig :: Config
defaultConfig = Config { getPool = undefined
                       , getPoolConnStr = undefined
                       , getQSemN = undefined
                       , getCounter = undefined
                       , getSrcUrl = undefined
                       , getSrcPort = undefined
                       , getMaxQSemN = undefined
                       }
