{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import Data.Text

type TestAPI = "test" :> Get '[PlainText] Text

main :: IO ()
main = run 3000 app

app :: Application
app = serve testAPI server

server :: Server TestAPI
server = return "test"

testAPI :: Proxy TestAPI
testAPI = Proxy
