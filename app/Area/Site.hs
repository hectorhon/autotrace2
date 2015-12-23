{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Area.Site where

import Servant
import AppM
import Data.Text

type AreaSite = Get '[PlainText] Text

areaSite :: Proxy AreaSite
areaSite = Proxy

areaServer :: ServerT AreaSite AppM
areaServer = return "area"
