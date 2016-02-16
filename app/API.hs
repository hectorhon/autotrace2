{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5
import Area.API
import Blc.API
import Apc.API
import Apc.Issue.API
import Block.API
import Lopc.Routes
import Search.API
import User.Routes
import Job.Routes

type Site = UserRoutes
       :<|> HomePage
       :<|> AreaSite
       :<|> BlcSite
       :<|> ApcSite
       :<|> ApcIssueSite
       :<|> JobRoutes
       :<|> BlockSite
       :<|> LopcRoutes
       :<|> SearchSite

type HomePage = Get '[HTML] Html
