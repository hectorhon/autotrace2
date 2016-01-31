{-# LANGUAGE OverloadedStrings #-}

module Lopc.Handlers where

import Servant
import Text.Blaze.Html5 hiding (head, map, a, b, i, select)
import Database.Esqueleto hiding (isNothing)
import Control.Monad.Trans.Class
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either
import Data.List as L (groupBy)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Time
import AppM
import Common.Responses
import Lopc.Types
import Lopc.Routes
import Lopc.Views
import Lopc.Links

lopcHandlers :: ServerT LopcRoutes AppM
lopcHandlers = viewLopcs
          :<|> viewLopcsOverview
          :<|> viewLopc
          :<|> toEditLopc
          :<|> editLopc
          :<|> deleteLopc

viewLopcs :: Maybe Integer -> AppM Html
viewLopcs year = do
  (year', _, _) <- fmap toGregorian (liftIO (relativeDay 0))
  let yyyy = maybe year' id year
  lopcs <- runDb $ select $ from $ \ i -> do
    where_ (    i ^. LopcReportedOn >=. val (fromGregorian yyyy 1 1)
            &&. i ^. LopcReportedOn <. val (fromGregorian (yyyy + 1) 1 1))
    orderBy [ asc (i ^. LopcReportedOn), asc (i ^. LopcArea1) ]
    return i
  let monthStats = zipWith ($) (map monthAggregateLopc [1..12])
                               (repeat (map entityVal lopcs))
  return $ lopcSummaryPage
    monthStats (filter (isNothing . lopcClosedOn . entityVal) lopcs)

monthAggregateLopc :: Int -> [Lopc] -> (Int, Int, Int, Int, Int)
monthAggregateLopc month lopcs = foldr
  (\ lopc (new, _, openHz, openNHz, closed) ->
     let om = monthOf (lopcReportedOn lopc)
         new' = if om == month then new + 1 else new
         open' = openHz' + openNHz'
         openHz' = if om > month || not (lopcHazardous lopc) then openHz
           else case lopcClosedOn lopc of
             Nothing -> openHz + 1
             Just cd -> if monthOf cd > month then openHz + 1 else openHz
         openNHz' = if om > month || lopcHazardous lopc then openNHz
           else case lopcClosedOn lopc of
             Nothing -> openNHz + 1
             Just cd -> if monthOf cd > month then openNHz + 1 else openNHz
         closed' = case lopcClosedOn lopc of
           Just d -> if monthOf d == month then closed + 1 else closed
           Nothing -> closed
     in (new', open', openHz', openNHz', closed')) (0, 0, 0, 0, 0) lopcs
  where monthOf = (\ (_, mm, _) -> mm) . toGregorian

viewLopcsOverview :: Maybe Integer -> AppM Html
viewLopcsOverview year = do
  (year', _, _) <- fmap toGregorian (liftIO (relativeDay 0))
  let yyyy = maybe year' id year
  lopcs <- runDb $ select $ from $ \ i -> do
    where_ (    i ^. LopcReportedOn >=. val (fromGregorian yyyy 1 1)
            &&. i ^. LopcReportedOn <. val (fromGregorian (yyyy + 1) 1 1))
    orderBy [ asc (i ^. LopcArea1), asc (i ^. LopcReportedOn) ]
    return i
  let majorLopcs = filter ((== MajorLopc). lopcClassification . entityVal) lopcs
  let minorLopcs = filter ((== MinorLopc). lopcClassification . entityVal) lopcs
  let otherOpenLopcs = (flip filter) lopcs (\ (Entity _ lopc) ->
        lopcClassification lopc == OtherLopc && lopcClosedOn lopc == Nothing)
  let majorLopcsSummary = groupAndSum (lopcArea1 . entityVal) majorLopcs
  let minorLopcsSummary = groupAndSum (lopcArea1 . entityVal) minorLopcs
  let otherOpenLopcsSummary = groupAndSum (lopcArea1 . entityVal) otherOpenLopcs
  return (lopcOverviewPage majorLopcsSummary majorLopcs
                           minorLopcsSummary minorLopcs
                           otherOpenLopcsSummary otherOpenLopcs)

groupAndSum :: Eq b => (a -> b) -> [a] -> [(b, Int)]
groupAndSum accessor = map toRow . L.groupBy hasSameProp
  where hasSameProp a b = accessor a == accessor b
        toRow xs = (accessor (head xs), length xs)

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
  mLopc <- runDb (get lid)
  if isNothing mLopc then (lift $ left err404) else runDb (replace lid lopc)
  redirect (viewLopcLink' lid)
  return undefined

deleteLopc :: Key Lopc -> AppM Text
deleteLopc lid = do
  runDb (deleteKey lid)
  return "deleted"
