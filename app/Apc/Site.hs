{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Apc.Site where

import Servant
import Text.Blaze.Html5 hiding (area)
import Database.Persist.Postgresql
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either
import Data.Maybe (isNothing)
import Data.Text (Text)
import AppM
import Schema
import Apc.API
import Apc.Views
import Area.Links
import Common.Responses

apcSite :: ServerT ApcSite AppM
apcSite = toCreateApc
     :<|> createApc
     :<|> viewApc
     :<|> updateApc
     :<|> deleteApc

toCreateApc :: Key Area -> AppM Html
toCreateApc aid = do
  mParent <- runDb $ selectFirst [AreaId ==. aid] []
  case mParent of Nothing     -> lift (left err404)
                  Just parent -> return (apcNewPage parent)

createApc :: Key Area -> Apc -> AppM Text
createApc pid apc = do
  _ <- runDb (insert apc)
  redirect (viewAreaLink' pid)
  return undefined

viewApc :: Key Area -> Key Apc -> AppM Html
viewApc pid aid = do
  mApc <- runDb $ selectFirst [ApcArea ==. pid, ApcId ==. aid] []
  case mApc of
    Nothing -> lift (left err404)
    Just apc -> do
      mParent <- runDb $ selectFirst [AreaId ==. pid] []
      case mParent of
        Nothing -> lift (left err404)
        Just parent -> return (apcIdPage apc parent)

updateApc :: Key Area -> Key Apc -> Apc -> AppM Text
updateApc pid aid apc = do
  mApc <- runDb $ selectFirst [ApcArea ==. pid, ApcId ==. aid] []
  if isNothing mApc then (lift $ left err404) else runDb (replace aid apc)
  redirect (viewAreaLink' pid)
  return undefined

deleteApc :: Key Area -> Key Apc -> AppM Text
deleteApc pid aid = do
  runDb $ deleteWhere [ApcArea ==. pid, ApcId ==. aid]
  return "deleted"
