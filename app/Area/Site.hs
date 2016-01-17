{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Area.Site where

import Servant
import Text.Blaze.Html5 hiding (head, area)
import Database.Persist.Postgresql
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Data.Text (Text)
import AppM
import Schema
import Area.API
import Area.Views
import Area.Links
import Common.Responses

areaSite :: ServerT AreaSite AppM
areaSite = toCreateArea
      :<|> createArea
      :<|> viewArea
      :<|> viewAreas
      :<|> toEditArea
      :<|> editArea
      :<|> deleteArea

toCreateArea :: Maybe (Key Area) -> AppM Html
toCreateArea mAid = case mAid of
  Nothing -> return (areaNewPage Nothing)
  Just aid -> runDb (selectFirst [AreaId ==. aid] []) >>= return . areaNewPage

createArea :: Area -> AppM Text
createArea area = do
  aid <- runDb (insert area)
  redirect (viewAreaLink' aid)
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
      blcs <- runDb $ selectList [BlcArea ==. aid] []
      apcs <- runDb $ selectList [ApcArea ==. aid] []
      return (areaIdPage area mParent children blcs apcs)

viewAreas :: AppM Html
viewAreas = do
   areas <- runDb $ selectList [AreaParent ==. Nothing] []
   return (areaHomePage areas)

toEditArea :: Key Area -> AppM Html
toEditArea aid = do
  mArea <- runDb (selectFirst [AreaId ==. aid] [])
  case mArea of
    Nothing -> lift (left err404)
    Just area -> do
      mParent <- case areaParent (entityVal area) of
        Nothing -> return Nothing
        Just pid -> runDb $ selectFirst [AreaId ==. pid] []
      return (areaEditPage area mParent)

editArea :: Key Area -> Area -> AppM Text
editArea aid area = do
  runDb (replace aid area)
  redirect (viewAreaLink' aid)
  return undefined

deleteArea :: Key Area -> AppM Text
deleteArea aid = do
  runDb (deleteCascade aid)
  return "deleted"
