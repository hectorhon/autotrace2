{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Area.Site where

import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5
import AppM

type AreaSite = Get '[HTML] Html

areaSite :: Proxy AreaSite
areaSite = Proxy

areaServer :: ServerT AreaSite AppM
areaServer = return $ docTypeHtml $ h1 "area"
