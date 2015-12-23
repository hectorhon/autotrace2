{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Servant
import Network.Wai
import Network.Wai.Handler.Warp
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
main = run 3000 (app defaultConfig)
