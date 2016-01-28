{-# LANGUAGE OverloadedStrings #-}

module Lopc.Views where

import Text.Blaze.Html5 as H hiding (head, a, b, map)
import Text.Blaze.Html5.Attributes as Ha hiding (open)
import Common.Views
import Database.Persist.Postgresql
import Control.Monad (forM_)
import Data.List
import Data.Time
import Lopc.Types

lopcSummaryPage :: [(Int, Int, Int, Int, Int)] -> [Entity Lopc] -> Html
lopcSummaryPage statistics lopcs = layout "LOPC" $ do
  h1 "LOPC"
  h2 "Summary"
  table ! class_ "list-table" $ do
    tr $ th "Month" >> th "New" >> th "Total open"
      >> th "Open (hazardous)" >> th "Open (non-hazardous)" >> th "Closed"
    forM_ (zip (map fst (months defaultTimeLocale)) statistics)
      (\ (month, (new, open, openHz, openNHz, closed)) -> tr $ do
         td (toHtml month)
         td (toHtml $ show new)
         td (toHtml $ show open)
         td (toHtml $ show openHz)
         td (toHtml $ show openNHz)
         td (toHtml $ show closed))
  let groupByArea1 =
        groupBy (\ (Entity _ a) (Entity _ b) -> lopcArea1 a == lopcArea1 b)
        . sortOn (lopcArea1 . entityVal)
  let lopcs' = groupByArea1 lopcs
  forM_ lopcs' $ \ ls -> do
    h2 (toHtml $ lopcArea1 (entityVal (head ls)))
    table $ do
      tr $ th "Area" >> th "Area" >> th "Description" >> th "Reported on"
      forM_ ls $ \ (Entity _ lopc) -> tr $ do
        td (toHtml $ lopcArea2 lopc)
        td (toHtml $ lopcArea3 lopc)
        td (toHtml $ lopcDescription lopc)
        td (toHtml $ show $ lopcReportedOn lopc)
