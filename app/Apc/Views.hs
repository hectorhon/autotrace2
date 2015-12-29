{-# LANGUAGE OverloadedStrings #-}

module Apc.Views where

import Text.Blaze.Html5 as H hiding (area)
import Text.Blaze.Html5.Attributes as Ha hiding (start)
import Database.Persist.Postgresql
import Common.Views
import Area.Links
import Apc.Links
import Schema

apcNewPage :: Entity Area -> Html
apcNewPage parent = layout "New advanced process controller" $ do
  h1 "New APC"
  apcForm parent Nothing

apcIdPage :: Entity Apc -> Entity Area -> Html
apcIdPage (Entity aid apc) parent = layout (apcName apc) $ do
  h1 $ toHtml (apcName apc)
  apcNavigation (entityKey parent) aid 1
  apcForm parent (Just apc)

apcNavigation :: Key Area -> Key Apc -> Int -> Html
apcNavigation pid aid = navigation
  [ ("Definition", viewApcLink pid aid)
  ]

apcForm :: Entity Area -> Maybe Apc -> Html
apcForm (Entity pid parent) mApc = H.form ! method "post" $ do
  field       "Name"                  "name"        apcName          mApc
  linkField   "Parent" (areaName parent) (viewAreaLink pid)
  hiddenField "area" (show $ fromSqlKey pid)
  field       "Uptime condition"      "uptimecond"  apcUptimeCond    mApc
  button "Save"
  maybe (cancelButton "apc-cancel-button")
        (deleteButton "apc-delete-button" . viewAreaLink' . apcArea) mApc
