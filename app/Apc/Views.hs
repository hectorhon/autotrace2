{-# LANGUAGE OverloadedStrings #-}

module Apc.Views where

import Text.Blaze.Html5 as H hiding (area)
import Text.Blaze.Html5.Attributes as Ha hiding (start)
import Database.Persist.Postgresql
import Control.Monad (forM_)
import Common.Views
import Area.Links
import Apc.Links
import Schema

apcNewPage :: Entity Area -> Html
apcNewPage parent = layout "New advanced process controller" $ do
  h1 "New APC"
  apcForm parent Nothing

apcIdPage :: Entity Apc -> Entity Area -> [Entity Cv] -> Html
apcIdPage (Entity aid apc) parent cvs = layout (apcName apc) $ do
  h1 $ toHtml (apcName apc)
  apcNavigation (entityKey parent) aid 1
  apcForm parent (Just apc)
  h2 "CVs"
  ul $ do
    forM_ cvs (\ (Entity cid cv) -> li $
      a ! href (viewApcCvLink (apcArea apc) aid cid) $ toHtml (cvName cv))
    li $ a ! href (toCreateApcCvLink (apcArea apc) aid) $ "New CV..."

apcNavigation :: Key Area -> Key Apc -> Int -> Html
apcNavigation pid aid = navigation
  [ ("Definition", viewApcLink pid aid)
  ]

apcCvNewPage :: Entity Apc -> Html
apcCvNewPage apc = layout "New CV" $ do
  h1 "New CV"
  apcCvForm apc Nothing

apcCvIdPage :: Entity Cv -> Entity Apc -> Html
apcCvIdPage (Entity cid cv) apc = layout (cvName cv) $ do
  h1 $ toHtml (cvName cv)
  cvNavigation (apcArea $ entityVal apc) (entityKey apc) cid 1
  apcCvForm apc (Just cv)

cvNavigation :: Key Area -> Key Apc -> Key Cv -> Int -> Html
cvNavigation aid apcId cid = navigation
  [ ("Definition", viewApcCvLink aid apcId cid)
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

apcCvForm :: Entity Apc -> Maybe Cv -> Html
apcCvForm (Entity aid apc) mCv = H.form ! method "post" $ do
  field "Name"               "name"    cvName    mCv
  linkField "APC" (apcName apc) (viewApcLink (apcArea apc) aid)
  hiddenField "area" (show $ fromSqlKey aid)
  field "Measurement tag"    "meastag" cvMeasTag mCv
  field "Set range high tag" "srhtag"  cvSrhTag  mCv
  field "Set range low tag"  "srltag"  cvSrlTag  mCv
  field "Prediction tag"     "predtag" cvPredTag mCv
  field "Selected tag"       "seltag"  cvSelTag  mCv
  button "Save"
  maybe (cancelButton "apc-cv-cancel-button")
        (deleteButton "apc-cv-delete-button"
        . viewApcLink' (apcArea apc) . cvApc)
        mCv
