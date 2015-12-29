{-# LANGUAGE OverloadedStrings #-}

module Blc.Views where

import Text.Blaze.Html5 as H hiding (area)
import Text.Blaze.Html5.Attributes as Ha hiding (start)
import Database.Persist.Postgresql
import Control.Monad (forM_, when)
import Common.Views
import Area.Links
import Area.Views
import Blc.Links
import Blc.Queries
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

areaBlcCalculatePage :: UTCTime -> UTCTime -> Entity Area -> Html
areaBlcCalculatePage start end (Entity aid area) =
  layout "Calculate controllers by area" $ do
    h1 $ toHtml (areaName area)
    areaNavigation aid 2
    p $ do
      H.span "Calculate controller performance in "
      a ! href (viewAreaLink aid) $ toHtml (areaName area)
      H.span ":"
    H.form ! method "post" $ do
      H.label $ do
        H.span "Start"
        datepicker "start-field" "start" (formatDay start)
      H.label $ do
        H.span "End"
        datepicker "end-field" "end" (formatDay end)
      button "Submit"
      cancelButton "area-blc-calculate-cancel-button"

areaBlcPage :: UTCTime -> UTCTime -> AreaBlcResult -> Html
areaBlcPage start end result =
  let AreaBlcResult area _ _ _ _ _ _ _ _ subareasBlcResult blcsResult = result
  in layout "Controller performance" $ do
    h1 (toHtml $ areaName $ entityVal area)
    areaNavigation (entityKey area) 2
    H.form ! class_ "line-form" ! method "get" $ do
      H.label $ do
        H.span "Start"
        datepicker "start-field" "start" (formatDay start)
      H.label $ do
        H.span "End"
        datepicker "end-field" "end" (formatDay end)
      button "Refresh"
      a ! href (toCalculateAreaBlcsLink (entityKey area)
                                        (utcToLocalDay start)
                                        (utcToLocalDay end)) $ "Recalculate"
    h2 "Summary"
    byAreasBlcResultTable [result] 
    when (not $ null subareasBlcResult) $ do
      h2 "Subareas"
      byAreasBlcResultTable subareasBlcResult
    when (not $ null blcsResult) $ do
      h2 "Controllers"
      blcsResultTable blcsResult

byAreasBlcResultTable :: [AreaBlcResult] -> Html
byAreasBlcResultTable results = table ! class_ "result-table" $ do
  col ! class_ "result-table-col-1"
  col ! class_ "result-table-col-2"
  col ! class_ "result-table-col-3"
  col ! class_ "result-table-col-4"
  col ! class_ "result-table-col-5"
  col ! class_ "result-table-col-6"
  col ! class_ "result-table-col-7"
  col ! class_ "result-table-col-8"
  tr $ do
    th ""
    th "Compliance"
    th "Quality"
    th "# Mode interv."
    th "# MV interv."
    th "# SP interv."
    th "MV sat."
    th "CV aff. by sat."
  let over x y = show x ++ " / " ++ show y in forM_ results
    (\ (AreaBlcResult (Entity aid area) compliance quality blcCount
       modeIntervCount mvIntervCount spIntervCount mvSat cvAffBySat _ _) ->
      tr $ do
        td $ a ! href (viewBlcsPerformanceDefaultDayLink aid)
               $ toHtml (areaName area)
        td $ bar "lightgreen" compliance blcCount (compliance `over` blcCount)
        td $ bar "lightblue" quality blcCount (quality `over` blcCount)
        td $ toHtml (show modeIntervCount)
        td $ toHtml (show mvIntervCount)
        td $ toHtml (show spIntervCount)
        td $ bar "orange" mvSat 100 ""
        td $ bar "orange" cvAffBySat 100 "")

blcsResultTable :: [BlcResult] -> Html
blcsResultTable results = table ! class_ "result-table" $ do
  col ! class_ "result-table-col-1"
  col ! class_ "result-table-col-2"
  col ! class_ "result-table-col-3"
  col ! class_ "result-table-col-4"
  col ! class_ "result-table-col-5"
  col ! class_ "result-table-col-6"
  col ! class_ "result-table-col-7"
  col ! class_ "result-table-col-8"
  tr $ do
    th ""
    th "Compliance"
    th "Quality"
    th "# Mode interv."
    th "# MV interv."
    th "# SP interv."
    th "MV sat."
    th "CV aff. by sat."
  forM_ results
    (\ (BlcResult (Entity bid blc) compliance quality
       modeIntervCount mvIntervCount spIntervCount mvSat cvAffBySat) -> tr $ do
      td $ a ! href (viewBlcLink (blcArea blc) bid) $ toHtml (blcName blc)
      td $ bar "lightgreen" compliance 100 ""
      td $ bar "lightblue" quality 100 ""
      td $ toHtml (show modeIntervCount)
      td $ toHtml (show mvIntervCount)
      td $ toHtml (show spIntervCount)
      td $ bar "orange" mvSat 100 ""
      td $ bar "orange" cvAffBySat 100 "")

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
