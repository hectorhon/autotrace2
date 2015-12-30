{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Apc.Site where

import Servant
import Text.Blaze.Html5 hiding (area, i, a)
import Database.Persist.Postgresql
import qualified Database.Esqueleto as E
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Data.Maybe (isNothing)
import Data.Text (Text)
import AppM
import Config
import Schema
import Time
import Apc.API
import Apc.Views
import Apc.Links
import Apc.Calculate
import Area.Links
import Common.Responses
import TimeSeriesData (getTSData)

apcSite :: ServerT ApcSite AppM
apcSite = toCreateApc
     :<|> createApc
     :<|> viewApc
     :<|> viewApcs
     :<|> updateApc
     :<|> deleteApc
     :<|> toCalculateApc
     :<|> calculateApc
     :<|> viewApcPerformance

     :<|> toCreateApcCv
     :<|> createApcCv
     :<|> viewApcCv
     :<|> updateApcCv
     :<|> deleteApcCv
     :<|> viewApcCvTrend

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
      cvs <- runDb $ selectList [CvApc ==. aid] []
      case mParent of
        Nothing -> lift (left err404)
        Just parent -> return (apcIdPage apc parent cvs)

viewApcs :: AppM Html
viewApcs = runDb $ selectList [] [] >>= return . apcsPage

updateApc :: Key Area -> Key Apc -> Apc -> AppM Text
updateApc pid aid apc = do
  mApc <- runDb $ selectFirst [ApcArea ==. pid, ApcId ==. aid] []
  if isNothing mApc then (lift $ left err404) else runDb (replace aid apc)
  redirect (viewAreaLink' pid)
  return undefined

deleteApc :: Key Area -> Key Apc -> AppM Text
deleteApc pid aid = do
  runDb $ deleteCascadeWhere [ApcArea ==. pid, ApcId ==. aid]
  return "deleted"

toCalculateApc :: Key Area -> Key Apc -> Maybe Day -> Maybe Day -> AppM Html
toCalculateApc aid apcId mStart mEnd = do
  mApc <- runDb $ selectFirst [ApcArea ==. aid, ApcId ==. apcId] []
  case mApc of
    Nothing  -> lift (left err404)
    Just apc -> do
      start <- maybe (liftIO $ relativeDay (-1)) (return . localDayToUTC) mStart
      end   <- maybe (liftIO $ relativeDay 0) (return . localDayToUTC) mEnd
      return (apcCalculatePage start end apc)

calculateApc :: Key Area -> Key Apc -> (Day, Day) -> AppM Text
calculateApc aid apcId (start, end) = do
  runDb (selectFirst [ApcArea ==. aid, ApcId ==. apcId] [])
  >>= maybe (lift $ left err404)
            (markCalculate (localDayToUTC start) (localDayToUTC end))
  >> redirect (viewApcLink' aid apcId)
  >> return undefined

viewApcPerformance :: Key Area -> Key Apc -> Maybe Day -> Maybe Day -> AppM Html
viewApcPerformance aid apcId mStart mEnd = do
  mApc <- runDb $ selectFirst [ApcArea ==. aid, ApcId ==. apcId] []
  case mApc of
    Nothing  -> lift (left err404)
    Just apc -> do
      start <- maybe (liftIO $ relativeDay (-1)) (return . localDayToUTC) mStart
      end   <- maybe (liftIO $ relativeDay 0) (return . localDayToUTC) mEnd
      uptimes <- runDb $ (flip selectList) [Asc ApcIntervalStart]
                 ([ApcIntervalApc ==. apcId] ++
                  (    [ApcIntervalStart >=. start, ApcIntervalStart <.  end]
                   ||. [ApcIntervalEnd   >.  start, ApcIntervalEnd   <=. end]
                   ||. [ApcIntervalStart <=. start, ApcIntervalEnd   >=. end]))
      issues <- runDb $ (flip selectList) [Asc ApcIssueStart]
                ([ApcIssueApc ==. apcId] ++
                 (    [ApcIssueStart  >=. start, ApcIssueStart <.  end]
                  ||. [ApcIssueEnd    >.  start, ApcIssueEnd   <=. end]
                  ||. [ApcIssueStart  <=. start, ApcIssueEnd   >=. end]))
      cvs <- runDb $ selectList [CvApc ==. apcId] [Asc CvName]
      cvExceeds <- runDb $ E.select $ E.from $
        \ (i `E.InnerJoin` cv `E.InnerJoin` a) -> do
          E.on (cv E.^. CvApc E.==. a E.^. ApcId)
          E.on (i E.^. CvIntervalCv E.==. cv E.^. CvId)
          E.where_ ((a E.^. ApcId E.==. E.val apcId) E.&&. (
                  (     (i E.^. CvIntervalStart E.>=. E.val start)
                  E.&&. (i E.^. CvIntervalStart E.<.  E.val end))
            E.||. (     (i E.^. CvIntervalEnd   E.>.  E.val start)
                  E.&&. (i E.^. CvIntervalEnd   E.<=. E.val end))
            E.||. (     (i E.^. CvIntervalStart E.<=. E.val start)
                  E.&&. (i E.^. CvIntervalEnd   E.>=. E.val end))
            ))
          return i
      return (apcPerformancePage apc start end uptimes issues cvs cvExceeds)



toCreateApcCv :: Key Area -> Key Apc -> AppM Html
toCreateApcCv aid apcId = do
  mApc  <- runDb $ selectFirst [ApcArea ==. aid, ApcId ==. apcId] []
  case mApc of
    Nothing -> lift (left err404)
    Just apc -> return (apcCvNewPage apc)

createApcCv :: Key Area -> Key Apc -> Cv -> AppM Text
createApcCv aid apcId cv = do
  _ <- runDb (insert cv)
  redirect (viewApcLink' aid apcId)
  return undefined

viewApcCv :: Key Area -> Key Apc -> Key Cv -> AppM Html
viewApcCv aid apcId cid = do
  mApc <- runDb $ selectFirst [ApcArea ==. aid, ApcId ==. apcId] []
  case mApc of
    Nothing -> lift (left err404)
    Just apc -> do
      mCv <- runDb $ selectFirst [CvApc ==. apcId, CvId ==. cid] []
      case mCv of
        Nothing -> lift (left err404)
        Just cv -> return (apcCvIdPage cv apc)

updateApcCv :: Key Area -> Key Apc -> Key Cv  -> Cv -> AppM Text
updateApcCv aid apcId cid cv = do
  mApc <- runDb $ selectFirst [ApcArea ==. aid, ApcId ==. apcId] []
  if isNothing mApc then lift (left err404)
  else do mCv <- runDb $ selectFirst [CvApc ==. apcId, CvId ==. cid] []
          if isNothing mCv then lift (left err404)
          else do runDb $ replace cid cv
                  redirect (viewApcLink' aid apcId)
                  return undefined

deleteApcCv :: Key Area -> Key Apc -> Key Cv -> AppM Text
deleteApcCv aid apcId cid = do
  mApc <- runDb $ selectFirst [ApcArea ==. aid, ApcId ==. apcId] []
  if isNothing mApc then lift (left err404)
  else do runDb $ deleteCascadeWhere [CvApc ==. apcId, CvId ==. cid]
          return "deleted"

viewApcCvTrend :: Key Area -> Key Apc -> Key Cv -> Maybe Day -> Maybe Day
               -> AppM Html
viewApcCvTrend aid apcId cid mStart mEnd = do
  mApc <- runDb $ selectFirst [ApcArea ==. aid, ApcId ==. apcId] []
  case mApc of
    Nothing -> lift (left err404)
    Just apc -> do
      mCv <- runDb $ selectFirst [CvApc ==. apcId, CvId ==. cid] []
      case mCv of
        Nothing -> lift (left err404)
        Just ecv@(Entity _ cv) -> do
          start  <- maybe (liftIO $ relativeDay (-1))
                          (return . localDayToUTC) mStart
          end    <- maybe (liftIO $ relativeDay 0)
                          (return . localDayToUTC) mEnd
          src    <- asks getSrcUrl
          port   <- asks getSrcPort
          let tags = [ cvSrlTag cv, cvMeasTag cv, cvSrhTag cv ]
          tsData <- liftIO (getTSData src port tags start end)
          return (apcCvTrendPage ecv apc tsData)
