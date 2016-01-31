{-# LANGUAGE OverloadedStrings #-}

module Lopc.Views where

import Text.Blaze.Html5 as H hiding (head, i, map, summary)
import Text.Blaze.Html5.Attributes as Ha hiding (open, summary)
import Data.Aeson (encode)
import Database.Persist.Postgresql
import Control.Monad (forM_, when)
import Data.Maybe (fromJust, isJust)
import Data.List (groupBy, sortOn)
import Data.Text (Text)
import Data.ByteString.Lazy.Char8 (unpack)
import Time
import Common.Views
import Lopc.Types
import Lopc.Links

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
        groupBy (\ (Entity _ i) (Entity _ j) -> lopcArea1 i == lopcArea1 j)
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

lopcOverviewPage :: [(Text, Int)] -> [Entity Lopc]
                 -> [(Text, Int)] -> [Entity Lopc]
                 -> [(Text, Int)] -> [Entity Lopc]
                 -> Html
lopcOverviewPage majorSum major minorSum minor otherOpenSum otherOpen =
  layout "LOPC" $ do
    h1 "LOPC overview"
    h2 "Major LOPCs"
    lopcTables majorSum major
    h2 "Minor LOPCs"
    lopcTables minorSum minor
    h2 "Open Other LOPCs"
    if null otherOpenSum then p "Hurray, nothing here!" else do
      H.div ! Ha.id "other-open-lopc-pie-chart"
            ! Ha.style "text-align:center;float:left;width:50%;"
            $ ""
      script (toHtml $ "var data = " ++ (unpack $ encode otherOpenSum) ++ ";")
      script ! src "/lopc.js" $ ""
      table ! class_ "list-table" ! Ha.style "float:right;width:50%;" $ do
        tr $ th "Area" >> th "Count"
        forM_ otherOpenSum (\ entry -> tr $ do
          td (toHtml $ fst entry)
          td (toHtml $ show $ snd entry))
      table ! class_ "list-table" $ do
        col ! class_ "other-lopc-table-col-1"
        col ! class_ "other-lopc-table-col-2"
        col ! class_ "other-lopc-table-col-3"
        col ! class_ "other-lopc-table-col-4"
        tr $ th "Area" >> th "Description" >> th "Fluid"
          >> th "Framework" >> th "Reported on"
        forM_ otherOpen (\ (Entity lid lopc) -> tr $ do
          td $ do
            H.div (toHtml $ lopcArea1 lopc)
            H.div (toHtml $ lopcArea2 lopc)
            H.div (toHtml $ lopcArea3 lopc)
          td (a ! href (viewLopcLink lid) $ toHtml $ lopcDescription lopc)
          td (toHtml $ lopcFluid lopc)
          td (toHtml $ lopcFramework lopc)
          td (toHtml $ lopcReportedOn lopc))
  where lopcTables summary lopcs =
          if null summary then p "Hurray, nothing here!" else do
            table ! class_ "list-table" $ do
              tr $ th "Area" >> th "Count"
              forM_ summary (\ entry -> tr $ do
                td (toHtml $ fst entry)
                td (toHtml $ show $ snd entry))
            table ! class_ "list-table" $ do
              col ! class_ "lopc-table-col-1"
              col ! class_ "lopc-table-col-2"
              col ! class_ "lopc-table-col-3"
              col ! class_ "lopc-table-col-4"
              col ! class_ "lopc-table-col-5"
              col ! class_ "lopc-table-col-6"
              tr $ th "Area" >> th "" >> th "" >> th "Description"
                >> th "Framework" >> th "Reported on"
              forM_ lopcs (\ (Entity lid lopc) -> tr $ do
                td (toHtml $ lopcArea1 lopc)
                td (toHtml $ lopcArea2 lopc)
                td (toHtml $ lopcArea3 lopc)
                td (a ! href (viewLopcLink lid) $ toHtml $ lopcDescription lopc)
                td (toHtml $ lopcFramework lopc)
                td (toHtml $ lopcReportedOn lopc))

lopcPage :: Entity Lopc -> Html
lopcPage (Entity lid lopc) = layout "View LOPC" $ do
  h1 (toHtml ("View LOPC #" ++ (show (fromSqlKey lid))))
  let (year, _, _) = toGregorian (lopcReportedOn lopc)
  navigation [("Back to overview", viewLopcsOverviewLink year)] 0
  lopcDD (Entity lid lopc)

lopcDD :: Entity Lopc -> Html
lopcDD (Entity lid lopc) = do
  editableH2 "Classification" (toEditLopcLink lid)
  table ! class_ "definition-table" $ do
    tr $ th "Description" >> td (toHtml $ lopcDescription lopc)
    tr $ th "Reported on" >> td (toHtml $ lopcReportedOn lopc)
    tr $ th "Classification" >> td (toHtml $ show $ lopcClassification lopc)
    tr $ th "Framework" >> td (toHtml $ lopcFramework lopc)
    tr $ th "Status" >> td (toHtml $ case lopcClosedOn lopc of
      Nothing        -> "Open"
      Just closeDate -> "Closed - " ++ formatDay closeDate)
  editableH2 "Location" (toEditLopcLink lid)
  table ! class_ "definition-table" $ do
    tr $ th "Area1" >> td (toHtml $ lopcArea1 lopc)
    tr $ th "Area2" >> td (toHtml $ lopcArea2 lopc)
    tr $ th "Area3" >> td (toHtml $ lopcArea3 lopc)
    tr $ th "Equipment tag" >> td (toHtml $ lopcEquipmentTag lopc)
    tr $ th "Equipment type" >> td (toHtml $ lopcEquipmentType lopc)
  editableH2 "Contents" (toEditLopcLink lid)
  table ! class_ "definition-table" $ do
    tr $ th "Fluid" >> td (toHtml $ lopcFluid lopc)
    tr $ th "Composition" >> td (toHtml $ lopcComposition lopc)
    tr $ th "Pressure" >> td (toHtml $ maybe 0 Prelude.id (lopcPressure lopc))
    tr $ th "Hazardous" >> td (toHtml $ lopcHazardous lopc)
  editableH2 "Misc. info" (toEditLopcLink lid)
  table ! class_ "definition-table" $ do
    tr $ th "Other ref." >> td (toHtml $ lopcOtherRef lopc)
    tr $ th "Remarks" >> td (toHtml $ lopcRemarks lopc)

lopcEditPage :: Lopc -> Html
lopcEditPage lopc = layout "Edit LOPC" $ do
  h1 "Edit LOPC"
  lopcForm (Just lopc)

lopcForm :: Maybe Lopc -> Html
lopcForm mLopc = H.form ! method "post" ! class_ "form-expanded" $ do
  fieldset $ do
    legend "Classification"
    field "Description" "description" lopcDescription mLopc
    H.label $ do
      H.span "Reported on"
      datepicker "reportedon-field" "reportedon"
                 (maybe "" (formatDay . lopcReportedOn) mLopc)
    selectField "Classification" "classification" lopcClassification mLopc
                [ (MinorLopc, "Minor LOPC")
                , (MajorLopc, "Major LOPC")
                , (OtherLopc, "Other LOPC") ]
    field "Framework" "framework" lopcFramework mLopc
    H.label $ do
      H.span "Closed?"
      (maybe Prelude.id
             (\ lopc -> if isJust (lopcClosedOn lopc) then (! Ha.checked "")
                        else Prelude.id)
              mLopc) $
        (input ! Ha.type_ "checkbox" ! Ha.id "isclosed-field" ! Ha.value "Yes")
    let f = if isJust mLopc && isJust (lopcClosedOn (fromJust mLopc))
            then Prelude.id else (! Ha.style "display:none;")
    f $ H.label ! Ha.id "closedon-attr-field" $ do
      H.span "Closing date"
      datepicker "closedon-field" "closedon"
                 (maybe "" (maybe "" formatDay . lopcClosedOn) mLopc)
    script $ " $('#isclosed-field').is(':checked') ?              \
             \   $('#closedon-attr-field').show()                 \
             \   : $('#closedon-attr-field').hide();              \
             \ $('#isclosed-field').change(function() {           \
             \   if ($('#isclosed-field').not(':checked')) {      \
             \     $('#closedon-field').val('');                  \
             \   }                                                \
             \   $('#closedon-attr-field').toggle();              \
             \ });                                                "
  fieldset $ do
    legend "Location"
    field "Area1" "area1" lopcArea1 mLopc
    field "Area2" "area2" lopcArea2 mLopc
    field "Area3" "area3" lopcArea3 mLopc
    field "Equipment tag" "equipmenttag" lopcEquipmentTag mLopc
    field "Equipment type" "equipmenttype" lopcEquipmentType mLopc
  fieldset $ do
    legend "Contents"
    field "Fluid" "fluid" lopcFluid mLopc
    field "Composition" "composition" lopcComposition mLopc
    field "Pressure" "pressure" (maybe "" show . lopcPressure) mLopc
    H.label $ do
      H.span "Hazardous?"
      (maybe Prelude.id
             (\ lopc -> if lopcHazardous lopc
                        then (! Ha.checked "")
                        else Prelude.id)
              mLopc) $
        (input ! Ha.type_ "checkbox" ! Ha.name "hazardous" ! Ha.value "Yes")
  fieldset $ do
    legend "Misc. info"
    field "Other ref." "otherref" lopcOtherRef mLopc
    field "Remarks" "remarks" lopcRemarks mLopc
  button "Save"
  when (isJust mLopc)
       (deleteButton "lopc-delete-button" viewLopcsOverviewDefaultYearLink')
  cancelButton "lopc-cancel-button"
