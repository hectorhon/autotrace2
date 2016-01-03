{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5
import Data.Text (Text)
import Area.API
import Blc.API
import Apc.API
import Apc.Issue.API
import Block.API

type Site = "migrate" :> Post '[PlainText] Text
       :<|> Get '[HTML] Html
       :<|> AreaSite
       :<|> BlcSite
       :<|> ApcSite
       :<|> ApcIssueSite
       :<|> BlockSite
