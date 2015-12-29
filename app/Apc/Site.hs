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
import Apc.Links
import Area.Links
import Common.Responses

apcSite :: ServerT ApcSite AppM
apcSite = toCreateApc
     :<|> createApc
     :<|> viewApc
     :<|> updateApc
     :<|> deleteApc
     :<|> toCreateApcCv
     :<|> createApcCv
     :<|> viewApcCv
     :<|> updateApcCv
     :<|> deleteApcCv

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
