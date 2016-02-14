{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Static
import Database.Persist.Postgresql
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Concurrent
import System.IO
import System.Environment (getArgs)
import Data.ByteString.Char8 (pack)
import Config
import AppM
import Area.Types
import Blc.Types
import Lopc.Types
import Schema
import API
import Area.Site
import Blc.Site
import Apc.Site
import Apc.Issue.Site
import Block.Site
import Lopc.Handlers
import Search.Site
import User.AuthMiddleware
import User.Types
import User.Handlers
import Home.Handlers
import Common.PackErrMiddleware (packErr)

server :: ServerT Site AppM
server = userHandlers
    :<|> homeHandlers
    :<|> areaSite
    :<|> blcSite
    :<|> apcSite
    :<|> apcIssueSite
    :<|> blockSite
    :<|> lopcHandlers
    :<|> searchSite

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
  cmdArgs    <- getArgs
  when (elem "migrate" cmdArgs) (migrateDb connPool)
  chan <- newChan
  _ <- forkIO (worker srcUrl srcPort (pack connString) chan)
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
                               , getChan = chan
                               }

worker :: String -> Int -> ConnectionString
       -> Chan (ReaderT (String, Int, SqlBackend, MVar (Int, Int)) IO ())
       -> IO ()
worker source port connString chan = runNoLoggingT $
  withPostgresqlConn connString $ \ conn -> lift $ forever $
    do thing <- readChan chan
       progress <- newMVar undefined
       tid <- forkIO (monitor progress)
       runReaderT thing (source, port, conn, progress)
       killThread tid

monitor :: MVar (Int, Int) -> IO ()
monitor progress = forever $ do
  threadDelay 3000000
  withMVar progress print

migrateDb :: ConnectionPool -> IO ()
migrateDb = runSqlPool (do runMigration migrateAll
                           runMigration migrateArea
                           runMigration migrateBlc
                           runMigration migrateLopc
                           runMigration migrateUser)
