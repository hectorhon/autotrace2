{-# LANGUAGE OverloadedStrings #-}

module Lopc.Handlers where

import Servant
import Text.Blaze.Html5 hiding (head, map, a, b, i, select)
import Database.Esqueleto
import Control.Monad.Trans.Class
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either
import Data.Maybe (listToMaybe)
import Data.List (sort, nub)
import Data.Text (Text)
import Time
import AppM
import Common.Responses
import Lopc.Types
import Lopc.Routes
import Lopc.Views
import Lopc.Links

lopcHandlers :: ServerT LopcRoutes AppM
lopcHandlers = viewLopcOverview
          :<|> viewLopcList
          :<|> toCreateLopc
          :<|> createLopc
          :<|> viewLopc
          :<|> toEditLopc
          :<|> editLopc
          :<|> deleteLopc

viewLopcOverview :: Maybe Integer -> AppM Html
viewLopcOverview year = do
  (year', _, _) <- fmap toGregorian (liftIO (relativeDay 0))
  let yyyy = maybe year' id year
  (lopcs, years, numAreas, numPastOpen) <- runDb $ do
    lopcs <- select $ from $ \ i -> do
      where_ (    i ^. LopcReportedOn >=. val (fromGregorian yyyy 1 1)
              &&. i ^. LopcReportedOn <. val (fromGregorian (yyyy + 1) 1 1))
      orderBy [asc (i ^. LopcArea1)]
      return i
    dates <- select $ distinct $ from $ \ i -> return (i ^. LopcReportedOn)
    numAreas <- select $ from $ \ i -> return (countDistinct (i ^. LopcArea1))
    numPastOpen <- select $ from $ \ i -> do
      where_ (    i ^. LopcReportedOn <. val (fromGregorian yyyy 1 1)
              &&. isNothing (i ^. LopcClosedOn))
      return countRows
    return ( lopcs
           , map (toYear . unValue) dates
           , maybe 0 unValue (listToMaybe numAreas)
           , maybe 0 unValue (listToMaybe numPastOpen))
  return (lopcOverviewPage lopcs yyyy (sort $ nub (year':years))
                           numAreas numPastOpen)
  where toYear d = let (y, _, _) = toGregorian d in y

viewLopcList :: Maybe Text -> AppM Html
viewLopcList mStatus = do
  let isOpen = maybe True ((==) "open") mStatus
  lopcs <- runDb $ select $ from $ \ i -> do
    where_ $ if isOpen then isNothing (i ^. LopcClosedOn)
             else not_ (isNothing (i ^. LopcClosedOn))
    orderBy [asc (i ^. LopcArea1), desc (i ^. LopcReportedOn)]
    return i
  return (lopcListPage lopcs isOpen)

toCreateLopc :: AppM Html
toCreateLopc = return lopcNewPage

createLopc :: Lopc -> AppM Text
createLopc lopc = do
  lid <- runDb (insert lopc)
  redirect (viewLopcLink' lid)
  return undefined

viewLopc :: Key Lopc -> AppM Html
viewLopc lid = do
  mLopc <- runDb (get lid)
  case mLopc of
    Nothing -> lift (left err404)
    Just lopc -> return (lopcPage (Entity lid lopc))

toEditLopc :: Key Lopc -> AppM Html
toEditLopc lid = do
  mLopc <- runDb (get lid)
  case mLopc of
    Nothing -> lift (left err404)
    Just lopc -> return (lopcEditPage lopc)

editLopc :: Key Lopc -> Lopc -> AppM Text
editLopc lid lopc = do
  runDb (replace lid lopc)
  redirect (viewLopcLink' lid)
  return undefined

deleteLopc :: Key Lopc -> AppM Text
deleteLopc lid = do
  runDb (deleteKey lid)
  return "deleted"
