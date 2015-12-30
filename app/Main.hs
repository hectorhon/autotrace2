{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Static
import Database.Persist.Postgresql
import Control.Monad.Logger
import Control.Concurrent
import System.IO
import Data.ByteString.Char8 (pack)
import Config
import AppM
import Schema
import API
import Common.Views (homePage)
import Area.Site
import Blc.Site
import Apc.Site
import Apc.Issue.Site

server :: ServerT Site AppM
server = (runDb (runMigration migrateAll) >> return "migrate requested")
    :<|> return homePage
    :<|> areaSite
    :<|> blcSite
    :<|> apcSite
    :<|> apcIssueSite

readerServer :: Config -> Server Site
readerServer cfg = enter (readerToEither cfg) server

app :: Config -> Application
app cfg = serve (Proxy :: Proxy Site) (readerServer cfg)

main :: IO ()
main = do
  connString <- openFile "connString.set" ReadMode >>= hGetLine
  connPool   <- runNoLoggingT $ createPostgresqlPool (pack connString) 1
  caching    <- initCaching PublicStaticCaching
  qsem       <- newQSem 100
  counter    <- newMVar 0
  srcFile    <- openFile "dataSource.set" ReadMode
  srcUrl     <- hGetLine srcFile
  srcPort    <- hGetLine srcFile >>= return . read
  run 3000 $ staticPolicy' caching (addBase "static")
           $ app defaultConfig { getPool = connPool
                               , getPoolConnStr = pack connString
                               , getQSem = qsem
                               , getCounter = counter
                               , getSrcUrl = srcUrl
                               , getSrcPort = srcPort
                               }
