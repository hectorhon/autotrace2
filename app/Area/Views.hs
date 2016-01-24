{-# LANGUAGE OverloadedStrings #-}

module Area.Views where

import Text.Blaze.Html5 as H hiding (area, link)
import Text.Blaze.Html5.Attributes as Ha
import Database.Persist.Postgresql
import Control.Monad
import Data.Maybe
import Common.Views
import Area.Links
import Blc.Links
import Apc.Links
import Area.Types
import Blc.Types
import Schema

areaHomePage :: String -> [Entity Area] -> Html
areaHomePage target areas = layout "Site list" $ do
  h1 "Select a plant"
  ul $ do
    forM_ areas
      (\ (Entity aid area) -> li $ a ! href (goto aid) $ toHtml (areaName area))
    li $ a ! href toCreateTopAreaLink $ "New plant..."
  where goto aid | target == "area" = viewAreaLink aid
                 | target == "blc"  = viewBlcsPerformanceDefaultDayLink aid
                 | otherwise        = viewAreaLink aid

areaNewPage :: Maybe (Entity Area) -> Html
areaNewPage mParent = layout "New area" $ do
  h1 "New area"
  areaForm mParent Nothing

areaIdPage :: Entity Area -> Maybe (Entity Area)
           -> [Entity Area] -> [Entity Blc] -> [Entity Apc]
           -> Html
areaIdPage (Entity aid area) mParent children blcs apcs =
  layout (areaName area) $ do
    H.h1 (toHtml $ areaName area)
    areaNavigation aid 1
    editableH2 "Definition" (toEditAreaLink aid)
    areaDD area mParent
    H.h2 "Subareas"
    ul $ do
      forM_ children (\ (Entity aid' area') -> li $
        a ! href (viewAreaLink aid') $ toHtml (areaName area'))
      li $ a ! href (toCreateAreaLink aid) $ "New subarea..."
    H.h2 "Base layer controllers"
    ul $ do
      forM_ blcs (\ (Entity bid blc) -> li $
        a ! href (viewBlcLink aid bid) $ toHtml (blcName blc))
      li $ a ! href (toCreateBlcLink aid) $ "New base layer controller..."
    H.h2 "APCs"
    ul $ do
      forM_ apcs (\ (Entity aid' apc) -> li $
        a ! href (viewApcLink aid aid') $ toHtml (apcName apc))
      li $ a ! href (toCreateApcLink aid) $ "New APC..."

areaEditPage :: Entity Area -> Maybe (Entity Area) -> Html
areaEditPage (Entity aid area) mParent = layout (areaName area) $ do
  H.h1 (toHtml $ areaName area)
  areaNavigation aid 1
  H.h2 "Definition - edit"
  areaForm mParent (Just area)

areaNavigation :: Key Area -> Int -> Html
areaNavigation aid = navigation
  [ ("Definition", viewAreaLink aid)
  , ("Base layer", viewBlcsPerformanceDefaultDayLink aid)
  ]

areaForm :: Maybe (Entity Area) -> Maybe Area -> Html
areaForm mParent mArea = let Entity kParent parent = fromJust mParent in
  H.form ! method "post" $ do
    field "Name"             "name"        areaName        mArea
    field "Description"      "description" areaDescription mArea
    when (isJust mParent) $ do
      linkField   "Parent" (areaName parent) (viewAreaLink kParent)
      hiddenField "parent" (show $ fromSqlKey kParent)
    field "Demand condition" "demandcond"  areaDemandCond  mArea
    button "Save"
    when (isJust mArea)
         (deleteButton "area-delete-button" $ viewAreasLink' "area")
    cancelButton "area-cancel-button"

areaDD :: Area -> Maybe (Entity Area) -> Html
areaDD area mParent = table ! class_ "definition-table" $ do
  tr $ th "Name" >> td (toHtml $ areaName area)
  tr $ th "Description" >> td (toHtml $ areaDescription area)
  case mParent of
    Nothing -> return ()
    Just (Entity _ parent) -> tr $ th "Parent" >> td (toHtml $ areaName parent)
  tr $ th "Demand condition" >> td (toHtml $ areaDemandCond area)
