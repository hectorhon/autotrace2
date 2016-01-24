{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Apc.Issue.Site where

import Servant
import Text.Blaze.Html5 hiding (area)
import Database.Persist.Postgresql
import qualified Database.Esqueleto as E
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Either
import Data.Maybe
import Data.Text (Text)
import AppM
import Area.Types
import Schema
import Time
import Apc.Issue.API
import Apc.Issue.Views
import Apc.Links
import Common.Responses

apcIssueSite :: ServerT ApcIssueSite AppM
apcIssueSite = toCreateApcIssue
          :<|> createApcIssue
          :<|> viewApcIssue
          :<|> viewApcIssues
          :<|> updateApcIssue
          :<|> deleteApcIssue

toCreateApcIssue :: Key Area -> Key Apc -> AppM Html
toCreateApcIssue aid apcId = do
  mApc <- runDb $ selectFirst [ApcArea ==. aid, ApcId ==. apcId] []
  day <- liftIO $ relativeDay' (-1)
  categories <- runDb $
                E.select $ E.distinct $ E.from $
                \ c -> return (c E.^. ApcIssueCategory)
  case mApc of
    Nothing -> lift (left err404)
    Just apc -> return (apcIssueNewPage apc day day (fmap E.unValue categories))

createApcIssue :: Key Area -> Key Apc -> ApcIssue -> AppM Text
createApcIssue aid apcId issue = do
  _ <- runDb (insert issue)
  redirect (viewApcLink' aid apcId)
  return undefined

viewApcIssue :: Key Area -> Key Apc -> Key ApcIssue -> AppM Html
viewApcIssue aid apcId iid = do
  mApc <- runDb $ selectFirst [ApcArea ==. aid, ApcId ==. apcId] []
  case mApc of
    Nothing -> lift (left err404)
    Just apc -> do
      mIssue <- runDb $
                selectFirst [ApcIssueApc ==. apcId, ApcIssueId ==. iid] []
      categories <- runDb $
                    E.select $ E.distinct $ E.from $
                    \ c -> return (c E.^. ApcIssueCategory)
      case mIssue of
        Nothing -> lift (left err404)
        Just issue -> return
                      (apcIssueIdPage issue apc (fmap E.unValue categories))

viewApcIssues :: Key Area -> Key Apc -> AppM Html
viewApcIssues aid apcId = do
  mApc <- runDb $ selectFirst [ApcArea ==. aid, ApcId ==. apcId] []
  case mApc of
    Nothing  -> lift (left err404)
    Just apc -> do
      issues <- runDb $ selectList [ApcIssueApc ==. apcId] []
      return (apcIssuesPage issues apc)

updateApcIssue :: Key Area -> Key Apc -> Key ApcIssue  -> ApcIssue -> AppM Text
updateApcIssue aid apcId iid issue = do
  mApc <- runDb $ selectFirst [ApcArea ==. aid, ApcId ==. apcId] []
  if isNothing mApc then lift (left err404)
  else do mIssue <- runDb $
                    selectFirst [ApcIssueApc ==. apcId, ApcIssueId ==. iid] []
          if isNothing mIssue then lift (left err404)
          else do runDb $ replace iid issue
                  redirect (viewApcLink' aid apcId)
                  return undefined

deleteApcIssue :: Key Area -> Key Apc -> Key ApcIssue -> AppM Text
deleteApcIssue aid apcId iid = do
  mApc <- runDb $ selectFirst [ApcArea ==. aid, ApcId ==. apcId] []
  if isNothing mApc then lift (left err404)
  else do runDb $ deleteCascadeWhere [ApcIssueApc ==. apcId, ApcIssueId ==. iid]
          return "deleted"
