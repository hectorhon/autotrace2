{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Area.Site where

import Servant
import Text.Blaze.Html5 hiding (head, area)
import Database.Persist.Postgresql
import Control.Monad.Trans.Either
import Control.Monad.Reader
import Data.ByteString.Char8 (pack)
import AppM
import Schema
import Area.API
import Area.Views

areaSite :: ServerT AreaSite AppM
areaSite = toCreateArea
        :<|> createArea
        :<|> viewArea
        :<|> viewAreas

toCreateArea :: Maybe (Key Area) -> AppM Html
toCreateArea mAid = case mAid of
  Nothing -> return (areaNewPage Nothing)
  Just aid -> runDb (selectFirst [AreaId ==. aid] []) >>= return . areaNewPage

createArea :: Area -> AppM Html
createArea area = do
   aid <- runDb (insert area)
   redirect $ show (fromSqlKey aid) ++ "/definition"
   return undefined

viewArea :: Key Area -> AppM Html
viewArea aid = do
  mArea <- runDb (selectFirst [AreaId ==. aid] [])
  case mArea of
    Nothing -> lift (left err404)
    Just area -> do
      children <- runDb $ selectList [AreaParent ==. Just aid] [Asc AreaName]
      mParent <- case areaParent (entityVal area) of
        Nothing -> return Nothing
        Just pid -> runDb $ selectFirst [AreaId ==. pid] []
      return (areaIdPage area mParent children)

viewAreas :: AppM Html
viewAreas = do
   areas <- runDb $ selectList [AreaParent ==. Nothing] []
   return (areaHomePage areas)

redirect :: String -> AppM ()
redirect url = lift $ left $ err301 { errHeaders = [("location", pack url)] }
