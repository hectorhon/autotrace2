{-# LANGUAGE OverloadedStrings #-}

module Blc.Views where

import Text.Blaze.Html5 as H hiding (area)
import Text.Blaze.Html5.Attributes as Ha hiding (start)
import Database.Persist.Postgresql
import Common.Views
import Area.Links
import Blc.Links
import Schema
import Time

blcNewPage :: Entity Area -> Html
blcNewPage parent = layout "New base layer controller" $ do
  h1 "New base layer controller"
  blcForm parent Nothing

blcIdPage :: Entity Blc -> Entity Area -> Html
blcIdPage (Entity bid blc) parent = layout (blcName blc) $ do
  h1 $ toHtml (blcName blc)
  blcNavigation (entityKey parent) bid 1
  blcForm parent (Just blc)

blcNavigation :: Key Area -> Key Blc -> Int -> Html
blcNavigation pid bid = navigation
  [ ("Definition", viewBlcLink pid bid)
  , ("Calculate", toCalculateBlcDefaultDayLink pid bid)
  ]

blcForm :: Entity Area -> Maybe Blc -> Html
blcForm (Entity pid parent) mBlc = H.form ! method "post" $ do
  field       "Name"                  "name"        blcName          mBlc
  field       "Description"           "description" blcDescription   mBlc
  linkField   "Parent" (areaName parent) (viewAreaLink pid)
  hiddenField "area" (show $ fromSqlKey pid)
  field       "Measurement tag"       "meastag"     blcMeasTag       mBlc
  field       "Setpoint tag"          "spttag"      blcSptTag        mBlc
  field       "Output tag"            "outtag"      blcOutTag        mBlc
  field       "Output saturate high"  "outmax"      blcOutMax        mBlc
  field       "Output saturate low"   "outmin"      blcOutMin        mBlc
  field       "Demand condition"      "demandcond"  blcDemandCond    mBlc
  field       "Uptime condition"      "uptimecond"  blcUptimeCond    mBlc
  selectField "Objective"             "objective"   blcObjective     mBlc
              [ (Near,  "Keep near setpoint")
              , (Above, "Keep above setpoint")
              , (Below, "Keep below setpoint") ]
  field       "Compliance margin"     "margin"      blcMargin        mBlc
  field       "Calc. MV interv. when" "calcmvicond" blcCalcMvICond   mBlc
  field       "Calc. SP interv. when" "calcspicond" blcCalcSpICond   mBlc
  button "Save"
  maybe (cancelButton "blc-cancel-button")
        (deleteButton "blc-delete-button" . viewAreaLink' . blcArea) mBlc

blcCalculatePage :: UTCTime -> UTCTime -> Entity Blc -> Html
blcCalculatePage start end (Entity bid blc) =
  layout "Single controller calculation" $ do
    h1 $ toHtml (blcName blc)
    blcNavigation (blcArea blc) bid 2
    H.form ! method "post" $ do
      H.label $ do
        H.span "Start"
        datepicker "start-field" "start" (formatDay start)
      H.label $ do
        H.span "End"
        datepicker "end-field" "end" (formatDay end)
      button "Submit"
      cancelButton "blc-calculate-cancel-button"
