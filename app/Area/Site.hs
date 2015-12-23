{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Area.Site where

import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5 hiding (head, area)
import Database.Persist.Postgresql
import Control.Monad (forM_)
import AppM
import Schema

type AreaSite = Get '[HTML] Html

areaSite :: Proxy AreaSite
areaSite = Proxy

areaServer :: ServerT AreaSite AppM
areaServer = do
  areas <- runDb $ selectList [] []
  return $ docTypeHtml $ body $ table $ do
    forM_ areas (\ (Entity _ area) -> tr $ do
      td $ toHtml (areaName area)
      td $ toHtml (areaDescription area))
