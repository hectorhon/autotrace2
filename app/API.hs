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
import Search.API
import User.Routes

type Site = "migrate" :> Post '[PlainText] Text
       :<|> UserRoutes
       :<|> HomePage
       :<|> AreaSite
       :<|> BlcSite
       :<|> ApcSite
       :<|> ApcIssueSite
       :<|> BlockSite
       :<|> SearchSite

type HomePage = Get '[HTML] Html
