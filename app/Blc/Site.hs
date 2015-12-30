{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Blc.Site where

import Servant
import Text.Blaze.Html5 hiding (area, map)
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
import Blc.Queries
import Blc.Calculate
import Area.Links
import Common.Responses

blcSite :: ServerT BlcSite AppM
blcSite = toCreateBlc
     :<|> createBlc
     :<|> viewBlc
     :<|> updateBlc
     :<|> deleteBlc
     :<|> toCalculateBlc
     :<|> viewBlcsPerformance
     :<|> toCalculateAreaBlcs
     :<|> calculateAreaBlcs

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
                     (return . localDayToUTC)
                     mStart
      end <- maybe (liftIO $ relativeDay 0)
                   (return . localDayToUTC)
                   mEnd
      return (blcCalculatePage start end blc)

viewBlcsPerformance :: Key Area -> Maybe Day -> Maybe Day -> AppM Html
viewBlcsPerformance aid mStart mEnd = do
  mArea <- runDb $ selectFirst [AreaId ==. aid] []
  case mArea of
    Nothing   -> lift (left err404)
    Just area -> do
      start <- maybe (liftIO $ relativeDay (-1))
                     (return . localDayToUTC)
                     mStart
      end <- maybe (liftIO $ relativeDay 0)
                   (return . localDayToUTC)
                   mEnd
      blcResultOf area start end >>= return . areaBlcPage start end

toCalculateAreaBlcs :: Key Area -> Maybe Day -> Maybe Day -> AppM Html
toCalculateAreaBlcs aid mStart mEnd = do
  mArea <- runDb $ selectFirst [AreaId ==. aid] []
  case mArea of
    Nothing   -> lift (left err404)
    Just area -> do
      start <- maybe (liftIO $ relativeDay (-1))
                     (return . localDayToUTC)
                     mStart
      end <- maybe (liftIO $ relativeDay 0)
                   (return . localDayToUTC)
                   mEnd
      return (areaBlcCalculatePage start end area)

calculateAreaBlcs :: Key Area -> (Day, Day) -> AppM Text
calculateAreaBlcs aid (start, end) = do
  blcs <- descendantBlcsOf' aid
  let days = map localDayToUTC [start..end]
  let days' = zip (init days) (tail days)
  let args = do blc <- blcs
                se  <- days'
                return (blc, se)
  forM_ args (\ (blc, (s, e)) -> markCalculate s e blc)
  redirect (viewAreaLink' aid)
  return undefined
