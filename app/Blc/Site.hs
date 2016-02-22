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
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Maybe
import Data.Text (Text)
import AppM
import Config
import Time
import Blc.Types
import Blc.API
import Blc.Views
import Blc.Queries
import Blc.Calculate
import Job.ScheduleJob
import Blc.Links
import Area.Types
import Area.Links
import User.Types
import Common.Responses
import TimeSeriesData

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

     :<|> listBlcsTags

     :<|> toCreateBlcLabel
     :<|> createBlcLabel
     :<|> viewBlcLabel
     :<|> viewBlcLabels
     :<|> toEditBlcLabel
     :<|> editBlcLabel
     :<|> deleteBlcLabel
     :<|> labelBlc
     :<|> unlabelBlc

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
      yesterday <- liftIO (relativeDay (-1))
      let start = maybe yesterday id mStart
      let end = maybe yesterday id mEnd
      return (blcCalculatePage start end blc)

calculateBlc :: Key Area -> Key Blc -> (Day, Day) -> Maybe (Entity User)
             -> AppM Text
calculateBlc aid bid (start, end) mUser = case mUser of
  Nothing -> lift (left err500)
  Just (Entity uid _) -> do
    scheduleJob "Calculate single blc" uid (job start end [bid])
    redirect (viewBlcLink' aid bid)
    return undefined

viewBlcsPerformance :: Key Area -> Maybe Day -> Maybe Day -> AppM Html
viewBlcsPerformance aid mStart mEnd = do
  mArea <- runDb (selectFirst [AreaId ==. aid] [])
  case mArea of
    Nothing -> lift (left err404)
    Just area -> do
      yesterday <- liftIO (relativeDay (-1))
      let start = maybe yesterday id mStart
      let end = maybe yesterday id mEnd
      if start > end
      then return (areaBlcPage start end area (Left negativeDateRangeMsg))
      else liftM (areaBlcPage start end area . Right)
                 (runDb (getResult start end area))
  where negativeDateRangeMsg = "Start cannot be later than end."

viewBlcBadActors :: Key Area -> Maybe Day -> Maybe Day
                 -> Maybe Double -> Maybe Double
                 -> AppM Html
viewBlcBadActors aid mStart mEnd mComplianceTargetPct mQualityTargetPct = do
  mArea <- runDb $ selectFirst [AreaId ==. aid] []
  case mArea of
    Nothing -> lift (left err404)
    Just area -> runDb $ do
      yesterday <- liftIO (relativeDay (-1))
      let start = maybe yesterday id mStart
      let end = maybe yesterday id mEnd
      let complianceTarget = maybe 0.95 (flip (/) 100) mComplianceTargetPct
      let qualityTarget = maybe 0.95 (flip (/) 100) mQualityTargetPct
      bids <- descendantBlcsOf aid
      badComplies <- getBadCompliances start end complianceTarget bids
      badQualities <- getBadQualities start end qualityTarget bids
      return (areaBlcBadActorsPage start end area
              complianceTarget qualityTarget badComplies badQualities)

toCalculateAreaBlcs :: Key Area -> Maybe Day -> Maybe Day -> AppM Html
toCalculateAreaBlcs aid mStart mEnd = do
  mArea <- runDb $ selectFirst [AreaId ==. aid] []
  case mArea of
    Nothing -> lift (left err404)
    Just area -> do
      yesterday <- liftIO (relativeDay (-1))
      let start = maybe yesterday id mStart
      let end = maybe yesterday id mEnd
      return (areaBlcCalculatePage start end area)

calculateAreaBlcs :: Key Area -> (Day, Day) -> Maybe (Entity User) -> AppM Text
calculateAreaBlcs aid (start, end) mUser = case mUser of
  Nothing -> lift (left err500)
  Just (Entity uid _) -> do
    bids <- runDb (descendantBlcsOf aid)
    scheduleJob "Calculate area blcs" uid (job start end bids)
    redirect (viewAreaLink' aid)
    return undefined

listBlcsTags :: Key Area -> AppM Html
listBlcsTags aid = do
  blcs <- runDb $ do
    bids <- descendantBlcsOf aid
    selectList [BlcId <-. bids] [Asc BlcName]
  LocalTime today _ <- liftIO (liftM (utcToLocalTime tz) getCurrentTime)
  let start = UTCTime (addDays (-1) today) 0
  let end = UTCTime today 0
  let (pblcs, parseErrors) = runWriter (mapM parseBlc blcs)
  let tags = concat (map listBlcTags pblcs)
  url <- reader getSrcUrl
  port <- reader getSrcPort
  sampleData <- liftIO (mapM (getTSPoints url port start end) tags)
  return (blcListTagsPage (zip tags (map (not . null) sampleData)) parseErrors)

toCreateBlcLabel :: AppM Html
toCreateBlcLabel = return blcLabelNewPage

createBlcLabel :: BlcLabel -> AppM Text
createBlcLabel blcLabel = do
  lid <- runDb (insert blcLabel)
  redirect (viewBlcLabelLink' lid)
  return undefined

viewBlcLabel :: Key BlcLabel -> AppM Html
viewBlcLabel lid = runDb (get lid)
  >>= maybe (lift $ left err404) (return . blcLabelPage) . fmap (Entity lid)

viewBlcLabels :: AppM Html
viewBlcLabels = runDb (selectList [] [Asc BlcLabelName])
                >>= return . blcLabelsPage

toEditBlcLabel :: Key BlcLabel -> AppM Html
toEditBlcLabel lid = runDb (get lid)
                     >>= maybe (lift $ left err404) (return . blcLabelEditPage)

editBlcLabel :: Key BlcLabel -> BlcLabel -> AppM Text
editBlcLabel lid blcLabel = do
  runDb (replace lid blcLabel)
  redirect (viewBlcLabelLink' lid)
  return undefined

deleteBlcLabel :: Key BlcLabel -> AppM Text
deleteBlcLabel lid = do
  runDb $ deleteCascadeWhere [BlcLabelId ==. lid]
  redirect viewBlcLabelsLink'
  return undefined

labelBlc = undefined
unlabelBlc = undefined
