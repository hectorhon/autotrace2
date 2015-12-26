{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Blc.Site where

import Servant
import Text.Blaze.Html5
import Database.Persist.Postgresql
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Text (Text)
import AppM
import Schema
import Time
import Blc.API
import Blc.Views
import Area.Links
import Common.Responses

blcSite :: ServerT BlcSite AppM
blcSite = toCreateBlc
     :<|> createBlc
     :<|> viewBlc
     :<|> updateBlc
     :<|> deleteBlc
     :<|> toCalculateBlc

toCreateBlc :: Key Area -> AppM Html
toCreateBlc aid = do
  mParent <- runDb $ selectFirst [AreaId ==. aid] []
  case mParent of Nothing     -> lift (left err404)
                  Just parent -> return (blcNewPage parent)

createBlc :: Key Area -> Blc -> AppM Text
createBlc pid blc = do
  _ <- runDb (insert blc)
  redirect (viewAreaLink' pid)
  return undefined

viewBlc :: Key Area -> Key Blc -> AppM Html
viewBlc pid bid = runMaybeT (do
  mBlc <- runDb $ selectFirst [BlcArea ==. pid, BlcId ==. bid] []
  guard (isJust mBlc)
  mParent <- runDb $ selectFirst [AreaId ==. pid] []
  guard (isJust mParent)
  return (fromJust mBlc, fromJust mParent))
  >>= maybe (lift $ left err404) (return . uncurry blcIdPage)

updateBlc :: Key Area -> Key Blc -> Blc -> AppM Text
updateBlc pid bid blc = do
  mBlc <- runDb $ selectFirst [BlcArea ==. pid, BlcId ==. bid] []
  if isNothing mBlc then (lift $ left err404) else runDb (replace bid blc)
  redirect (viewAreaLink' pid)
  return undefined

deleteBlc :: Key Area -> Key Blc -> AppM Text
deleteBlc pid bid = do
  runDb $ deleteWhere [BlcArea ==. pid, BlcId ==. bid]
  return "deleted"

toCalculateBlc :: Key Area -> Key Blc -> Maybe Day -> Maybe Day -> AppM Html
toCalculateBlc pid bid mStart mEnd = do
  mBlc <- runDb $ selectFirst [BlcArea ==. pid, BlcId ==. bid] []
  case mBlc of
    Nothing  -> lift (left err404)
    Just blc -> do
      start <- maybe (liftIO $ relativeDay (-1))
                     (return . flip UTCTime 0)
                     mStart
      end <- maybe (liftIO $ relativeDay 0)
                   (return . flip UTCTime 0)
                   mEnd
      return (blcCalculatePage start end blc)
