module Config where

import Database.Persist.Postgresql

data Config = Config { getPool :: ConnectionPool
                     }

defaultConfig :: Config
defaultConfig = Config { getPool = undefined
                       }
