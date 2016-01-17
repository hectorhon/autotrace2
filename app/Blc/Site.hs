{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Blc.Site where

import Servant
import Text.Blaze.Html5 hiding (area, map)
import Database.Persist.Postgresql
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
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
import Blc.Calculate as B
import Blc.Links
import Area.Links
import Area.Calculate as A
import Common.Responses

blcSite :: ServerT BlcSite AppM
blcSite = toCreateBlc
     :<|> createBlc
     :<|> viewBlc
     :<|> toEditBlc
     :<|> editBlc
     :<|> deleteBlc
     :<|> toCalculateBlc
     :<|> calculateBlc
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
viewBlc pid bid = do
  mBlc <- runDb $ selectFirst [BlcArea ==. pid, BlcId ==. bid] []
  case mBlc of
    Nothing  -> lift (left err404)
    Just blc -> do
      mParent <- runDb $ selectFirst [AreaId ==. pid] []
      case mParent of
        Nothing   -> lift (left err404)
        Just area -> return (blcIdPage blc area)

toEditBlc :: Key Area -> Key Blc -> AppM Html
toEditBlc pid bid = do
  mBlc <- runDb $ selectFirst [BlcArea ==. pid, BlcId ==. bid] []
  case mBlc of
    Nothing -> lift (left err404)
    Just blc -> do
      mParent <- runDb $ selectFirst [AreaId ==. pid] []
      case mParent of
        Nothing -> lift (left err404)
        Just area -> return (blcEditPage blc area)

editBlc :: Key Area -> Key Blc -> Blc -> AppM Text
editBlc pid bid blc = do
  mBlc <- runDb $ selectFirst [BlcArea ==. pid, BlcId ==. bid] []
  if isNothing mBlc then (lift $ left err404) else runDb (replace bid blc)
  redirect (viewBlcLink' pid bid)
  return undefined

deleteBlc :: Key Area -> Key Blc -> AppM Text
deleteBlc pid bid = do
  runDb $ deleteCascadeWhere [BlcArea ==. pid, BlcId ==. bid]
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

calculateBlc :: Key Area -> Key Blc -> (Day, Day) -> AppM Text
calculateBlc aid bid (start, end) =
  let start' = localDayToUTC start
      end'   = localDayToUTC end
  in do A.markCalculateASD start' end' aid
        B.markCalculate start' end' bid
        redirect (viewBlcLink' aid bid)
        return undefined

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
      let badComplies = catMaybes $ flip map compliances $ \ (k, mv) -> do
            case mv of Nothing -> Nothing
                       Just v  -> if v < complianceTarget then Just (k, v)
                                  else Nothing
      let badQualities = catMaybes $ flip map qualities $ \ (k, mv) ->
            case mv of Nothing -> Nothing
                       Just v  -> if v < qualityTarget then Just (k, v)
                                  else Nothing
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
calculateAreaBlcs aid (start, end) =
  let start' = localDayToUTC start
      end'   = localDayToUTC end
  in do bids <- descendantBlcsOf aid
        A.markCalculateASD start' end' aid
        forM_ bids (B.markCalculate start' end')
        redirect (viewAreaLink' aid)
        return undefined
