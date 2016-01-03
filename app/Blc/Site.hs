{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Blc.Site where

import Servant
import Text.Blaze.Html5 hiding (area)
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
     :<|> viewBlcBadActors
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
  start <- maybe (liftIO $ relativeDay (-1))
                 (return . localDayToUTC)
                 mStart
  end <- maybe (liftIO $ relativeDay 0)
               (return . localDayToUTC)
               mEnd
  mResult <- getResult start end aid
  case mResult of
    Nothing -> lift (left err404)
    Just (areaResult, subareaResults, blcResults) -> return $
      areaBlcPage start end areaResult subareaResults blcResults

viewBlcBadActors :: Key Area -> Maybe Day -> Maybe Day
                 -> Maybe Double -> Maybe Double
                 -> AppM Html
viewBlcBadActors aid mStart mEnd mComplianceTargetPct mQualityTargetPct = do
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
      bids <- descendantBlcsOf aid
      compliances <- getCompliances start end bids
      qualities <- getQualities start end bids
      let complianceTarget = maybe 0.95 (flip (/) 100) mComplianceTargetPct
      let qualityTarget = maybe 0.95 (flip (/) 100) mQualityTargetPct
      let badComplies = filter ((< complianceTarget) . snd) compliances
      let badQualities = filter ((< qualityTarget) . snd) qualities
      let getBlc (bid, v) = runDb (get bid)
            >>= maybe (return Nothing)
                      (\ blc -> return (Just (Entity bid blc, v)))
      badComplies' <- liftM catMaybes $ mapM getBlc badComplies
      badQualities' <- liftM catMaybes $ mapM getBlc badQualities
      return (areaBlcBadActorsPage start end area
              complianceTarget qualityTarget badComplies' badQualities')

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
  bids <- descendantBlcsOf aid
  forM_ bids (markCalculate (localDayToUTC start) (localDayToUTC end))
  redirect (viewAreaLink' aid)
  return undefined
