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
import Data.Text (Text)
import Data.ByteString.Char8 (pack)
import Config
import AppM
import Schema
import API
import Area.Site
import Blc.Site
import Apc.Site
import Apc.Issue.Site
import Block.Site
import Search.Site
import User.AuthMiddleware
import User.Types
import User.Handlers
import Home.Handlers
import Common.PackErrMiddleware (packErr)

server :: ServerT Site AppM
server = migrateSite
    :<|> userHandlers
    :<|> homeHandlers
    :<|> areaSite
    :<|> blcSite
    :<|> apcSite
    :<|> apcIssueSite
    :<|> blockSite
    :<|> searchSite

migrateSite :: AppM Text
migrateSite = do
  runDb (runMigration migrateAll >> runMigration migrateUser)
  return "migrate requested"

readerServer :: Config -> Server Site
readerServer cfg = enter (readerToEither cfg) server

app :: Config -> Application
app cfg = serve (Proxy :: Proxy Site) (readerServer cfg)

main :: IO ()
main = do
  connString <- openFile "connString.set" ReadMode >>= hGetLine
  connPool   <- runNoLoggingT $ createPostgresqlPool (pack connString) 5
  caching    <- initCaching PublicStaticCaching
  let maxQSemN = 10
  qsemn      <- newQSemN maxQSemN
  counter    <- newMVar 0
  srcFile    <- openFile "dataSource.set" ReadMode
  srcUrl     <- hGetLine srcFile
  srcPort    <- hGetLine srcFile >>= return . read
  run 3000 $ staticPolicy' caching (addBase "static")
           $ auth connPool
           $ packErr
           $ app defaultConfig { getPool = connPool
                               , getPoolConnStr = pack connString
                               , getQSemN = qsemn
                               , getCounter = counter
                               , getSrcUrl = srcUrl
                               , getSrcPort = srcPort
                               , getMaxQSemN = maxQSemN
                               }
