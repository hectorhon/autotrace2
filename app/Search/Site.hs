{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Search.Site where

import Servant
import Text.Blaze.Html5 hiding (map)
import Database.Persist.Postgresql
import Control.Monad (liftM)
import AppM
import Area.Types
import Blc.Types
import Search.Types
import Search.API
import Search.Views

searchSite :: ServerT SearchSite AppM
searchSite = search

search :: Maybe String -> AppM Html
search mpname = do
  results <- case mpname of
    Nothing -> return []
    Just pname -> do
      areas <- liftM (map pack) $ runDb $ selectList [AreaName ==. pname] []
      blcs <- liftM (map pack) $ runDb $ selectList [BlcName ==. pname] []
      return (concat [areas, blcs])
  return (searchPage mpname results)
