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
import System.IO
import Data.ByteString.Char8 (pack)
import Config
import AppM
import Area.Site

type Site = "area" :> AreaSite

site :: Proxy Site
site = Proxy

server :: ServerT Site AppM
server = areaServer

readerServer :: Config -> Server Site
readerServer cfg = enter (readerToEither cfg) server

app :: Config -> Application
app cfg = serve site (readerServer cfg)

main :: IO ()
main = do
  connString <- openFile "connString.set" ReadMode >>= hGetLine
  connPool   <- runNoLoggingT $ createPostgresqlPool (pack connString) 1
  caching    <- initCaching PublicStaticCaching
  run 3000 $ staticPolicy' caching (addBase "static")
           $ app defaultConfig { getPool = connPool }
