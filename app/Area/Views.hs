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
import Schema

areaHomePage :: [Entity Area] -> Html
areaHomePage areas = layout "Site list" $ do
  h1 "Select a plant"
  ul $ do
    forM_ areas (\ (Entity aid area) ->
      li $ a ! href (viewBlcsPerformanceDefaultDayLink aid)
             $ toHtml (areaName area))
    li $ a ! href toCreateTopAreaLink $ "New plant..."

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
    H.h2 "Definition"
    areaForm mParent (Just area)
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

areaNavigation :: Key Area -> Int -> Html
areaNavigation aid = navigation
  [ ("Definition", viewAreaLink aid)
  , ("Base layer", viewBlcsPerformanceDefaultDayLink aid)
  ]


areaForm :: Maybe (Entity Area) -> Maybe Area -> Html
areaForm mParent mArea = let
  Entity kParent parent = fromJust mParent
  pid = show (fromSqlKey kParent)
  in H.form ! method "post" $ do
        H.label $ do
          H.span "Name"
          input ! Ha.name "name"
                ! Ha.value (stringValue (maybe "" areaName mArea))
        H.label $ do
          H.span "Description"
          input ! Ha.name "description"
                ! Ha.value (stringValue (maybe "" areaDescription mArea))
        when (isJust mParent) (do
          H.label $ do
            H.span "Parent"
            a ! class_ "input-link"
              ! href (viewAreaLink $ entityKey $ fromJust mParent)
              $ toHtml (areaName parent)
          input ! Ha.name "parent"
                ! Ha.type_ "hidden"
                ! Ha.value (stringValue pid))
        button "Save"
        if isJust mArea then do
          button ! Ha.id "area-delete-button" $ "Delete"
          script $ toHtml $
            " $('#area-delete-button').click(function(e) { \
            \   e.preventDefault(); \
            \   $.ajax({ \
            \     method: 'DELETE', \
            \     url: window.location, \
            \     success: function() { \
            \       window.location.replace('"++viewAreasLink'++"'); \
            \     } \
            \   }) \
            \ }); "
        else do
          button ! Ha.id "area-cancel-button" $ "Cancel"
          script " $('#area-cancel-button').click(function(e) { \
                 \   e.preventDefault(); \
                 \   window.location.href = document.referrer; \
                 \ }); "
