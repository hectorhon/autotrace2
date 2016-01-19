{-# LANGUAGE OverloadedStrings #-}

module Blc.Views where

import Text.Blaze.Html5 as H hiding (area, q)
import Text.Blaze.Html5.Attributes as Ha hiding (start)
import Database.Persist.Postgresql
import Data.List (sortOn)
import Control.Monad (forM_, when)
import Common.Views
import Area.Types
import Area.Links
import Area.Views
import Blc.Types
import Blc.Links
import Blc.Queries
import Time

blcNewPage :: Entity Area -> Html
blcNewPage parent = layout "New base layer controller" $ do
  h1 "New base layer controller"
  blcForm parent Nothing

blcIdPage :: Entity Blc -> Entity Area -> Html
blcIdPage (Entity bid blc) parent = layout (blcName blc) $ do
  h1 $ toHtml (blcName blc)
  blcNavigation (entityKey parent) bid 1
  editableH2 "Definition" (toEditBlcLink (entityKey parent) bid)
  blcDD blc parent

blcEditPage :: Entity Blc -> Entity Area -> Html
blcEditPage (Entity bid blc) parent = layout (blcName blc) $ do
  h1 $ toHtml (blcName blc)
  blcNavigation (entityKey parent) bid 1
  h2 "Definition - edit"
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

areaBlcBadActorsPage :: UTCTime -> UTCTime -> Entity Area -> Double -> Double
                     -> [(Entity Blc, Double)] -> [(Entity Blc, Double)]
                     -> Html
areaBlcBadActorsPage start end (Entity aid area)
                     complianceTarget qualityTarget
                     badComplies badQualities =
  layout "Bad actors" $ do
    h1 $ toHtml (areaName area)
    areaNavigation aid 2
    h2 "Bad actors - Compliance"
    p $ toHtml $ "Controllers with compliance below "
                 ++ (show $ roundTo 1 $ complianceTarget * 100)
                 ++ " % (" ++ formatDay start ++ " - " ++ formatDay end ++ ")"
    if null badComplies then p "Hurray, nothing here!"
    else table ! class_ "list-table" $ do
      col ! class_ "bad-actors-table-col-1"
      col ! class_ "bad-actors-table-col-2"
      col ! class_ "bad-actors-table-col-3"
      col ! class_ "bad-actors-table-col-4"
      tr $ th "#" >> th "Controller" >> th "Description" >> th "Compliance (%)"
      forM_ (zip [1..] $ reverse $ sortOn snd badComplies)
            (\ (index, (Entity bid blc, compliance)) -> tr $ do
               td $ toHtml (show (index :: Int))
               td $ a ! href (viewBlcLink (blcArea blc) bid)
                      $ toHtml (blcName blc)
               td $ toHtml (blcDescription blc)
               td $ toHtml (show $ (roundTo 1 $ compliance * 100)))
    h2 "Bad actors - Quality"
    p $ toHtml $ "Controllers with quality below "
                 ++ (show $ roundTo 1 $ qualityTarget * 100)
                 ++ " % (" ++ formatDay start ++ " - " ++ formatDay end ++ ")"
    if null badQualities then p "Hurray, nothing here!"
    else table ! class_ "list-table" $ do
      col ! class_ "bad-actors-table-col-1"
      col ! class_ "bad-actors-table-col-2"
      col ! class_ "bad-actors-table-col-3"
      col ! class_ "bad-actors-table-col-4"
      forM_ (zip [1..] $ reverse $ sortOn snd badQualities)
            (\ (index, (Entity bid blc, quality)) -> tr $ do
                 td $ toHtml (show (index :: Int))
                 td $ a ! href (viewBlcLink (blcArea blc) bid)
                        $ toHtml (blcName blc)
                 td $ toHtml (blcDescription blc)
                 td $ toHtml (show $ (roundTo 1 $ quality * 100)))

areaBlcPage :: UTCTime -> UTCTime -> AreaResult -> [AreaResult] -> [BlcResult]
            -> Html
areaBlcPage start end
            areaResult@(AreaResult (Entity aid area) _ _ _ _ _ _ _ _)
            subareaResults blcResults =
  layout "Controller performance" $ do
    h1 (toHtml $ areaName area)
    areaNavigation aid 2
    H.form ! class_ "line-form" ! method "get" $ do
      H.label $ do
        H.span "Start"
        datepicker "start-field" "start" (formatDay start)
      H.label $ do
        H.span "End"
        datepicker "end-field" "end" (formatDay end)
      button "Refresh"
      a ! href (toCalculateAreaBlcsLink
                aid (utcToLocalDay start) (utcToLocalDay end)) $ "Recalculate"
    h2 "Summary"
    byAreasBlcResultTable start end [areaResult] 
    p $ do
      a ! href (viewBlcBadActorsLink
                aid (utcToLocalDay start) (utcToLocalDay end) 95 95)
        $ "View all bad actors"
      case areaParent area of
        Nothing -> return ()
        Just parent -> do
          H.span " "
          a ! href (viewBlcsPerformanceLink parent
                    (utcToLocalDay start) (utcToLocalDay end))
            $ "Up one level"
    when (not $ null subareaResults) $ do
      h2 "Subareas"
      byAreasBlcResultTable start end subareaResults
    when (not $ null blcResults) $ do
      h2 "Controllers"
      blcsResultTable blcResults

byAreasBlcResultTable :: UTCTime -> UTCTime -> [AreaResult] -> Html
byAreasBlcResultTable start end results = table ! class_ "result-table" $ do
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
    (\ (AreaResult (Entity aid area) compliance quality blcCount
       modeIntervCount mvIntervCount spIntervCount mvSat cvAffBySat) ->
      tr $ do
        td $ a ! href (viewBlcsPerformanceLink
                       aid (utcToLocalDay start) (utcToLocalDay end))
               $ toHtml (areaName area)
        td $ bar "lightgreen" compliance blcCount (compliance `over` blcCount)
        td $ bar "lightblue" quality blcCount (quality `over` blcCount)
        td $ toHtml (show modeIntervCount)
        td $ toHtml (show mvIntervCount)
        td $ toHtml (show spIntervCount)
        td $ bar "orange" mvSat 1 ""
        td $ bar "orange" cvAffBySat 1 "")

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
      td $ a ! href (viewBlcLink (blcArea blc) bid)
             ! Ha.title (stringValue $ blcDescription blc)
             $ toHtml (blcName blc)
      case compliance of
        Just c  -> td $ bar "lightgreen" c          1 ""
        Nothing -> td $ bar "green"      (1 :: Int) 1 ""
      case quality of
        Just q  -> td $ bar "lightblue" q          1 ""
        Nothing -> td $ bar "blue"      (1 :: Int) 1 ""
      td $ toHtml (show modeIntervCount)
      td $ toHtml (show mvIntervCount)
      td $ toHtml (show spIntervCount)
      td $ bar "orange" mvSat 1 ""
      td $ bar "orange" cvAffBySat 1 "")

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
  maybe (return ())
        (deleteButton "blc-delete-button"
         . viewBlcsPerformanceDefaultDayLink' . blcArea)
        mBlc
  cancelButton "blc-cancel-button"

blcDD :: Blc -> Entity Area -> Html
blcDD blc (Entity pid parent) = table ! class_ "definition-table" $ do
  tr $ th "Name" >> td (toHtml $ blcName blc)
  tr $ th "Description" >> td (toHtml $ blcDescription blc)
  tr $ do
    th "Parent"
    td $ a ! href (viewBlcsPerformanceDefaultDayLink pid)
           $ (toHtml $ areaName parent)
  tr $ th "Measurement tag" >> td (toHtml $ blcMeasTag blc)
  tr $ th "Setpoint tag" >> td (toHtml $ blcSptTag blc)
  tr $ th "Output tag" >> td (toHtml $ blcOutTag blc)
  tr $ th "Output saturate high" >> td (toHtml $ blcOutMax blc)
  tr $ th "Output saturate low" >> td (toHtml $ blcOutMin blc)
  tr $ th "Demand condition" >> td (toHtml $ blcDemandCond blc)
  tr $ th "Uptime condition" >> td (toHtml $ blcUptimeCond blc)
  tr $ th "Objective" >> td (toHtml $ show $ blcObjective blc)
  tr $ th "Compliance margin" >> td (toHtml $ blcMargin blc)
  tr $ th "Calc. MV interv. when" >> td (toHtml $ blcCalcMvICond blc)
  tr $ th "Calc. SP interv. when" >> td (toHtml $ blcCalcSpICond blc)
