{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Servant
import Data.Text (Text)
import Area.API

type Site = "migrate" :> Post '[PlainText] Text
       :<|> AreaSite
