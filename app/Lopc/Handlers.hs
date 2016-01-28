module Lopc.Handlers where

import Servant
import Text.Blaze.Html5 hiding (map, a, b)
import Database.Persist.Postgresql
import Control.Monad.IO.Class (liftIO)
import Data.Maybe
import Time
import AppM
import Lopc.Types
import Lopc.Routes
import Lopc.Views

lopcHandlers :: ServerT LopcRoutes AppM
lopcHandlers = viewLopcs

viewLopcs :: Maybe Integer -> AppM Html
viewLopcs year = do
  (year', _, _) <- fmap toGregorian (liftIO (relativeDay 0))
  let yyyy = maybe year' id year
  lopcs <- runDb $ selectList
    [ LopcReportedOn >=. (fromGregorian yyyy 1 1)
    , LopcReportedOn <. (fromGregorian (yyyy + 1) 1 1) ]
    [ Asc LopcReportedOn, Asc LopcArea1 ]
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
