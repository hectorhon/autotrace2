{-# LANGUAGE OverloadedStrings #-}

module Lopc.Views where

import Prelude as P
import Text.Blaze.Html5 as H hiding (head, i, map, summary, area)
import Text.Blaze.Html5.Attributes as Ha hiding (open, summary)
import Database.Persist.Postgresql hiding (count)
import Control.Monad (forM_, when)
import Data.Function (on)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.List (groupBy, sortOn)
import Data.Text (unpack)
import Time
import Common.Views
import Lopc.Types
import Lopc.Links

lopcOverviewPage :: [Entity Lopc] -> Integer -> [Integer] -> Int -> Int -> Html
lopcOverviewPage lopcs year years numAreas numPastOpen = layout "LOPC" $ do
  h1 "LOPC"
  lopcNavigation 1
  h2 "YTD summary"
  H.form ! class_ "line-form" ! Ha.style "text-align:left;" $
    selectField "Year" "year" P.id (Just year) (zip years (map show years))
      ! onchange "this.form.submit()"
  let summary = let lopcs' = map entityVal lopcs
                    grouped= [ filter ((== MajorLopc).lopcClassification) lopcs'
                             , filter ((== MinorLopc).lopcClassification) lopcs'
                             , filter ((== OtherLopc).lopcClassification) lopcs'
                             ]
                    isOpen = isNothing . lopcClosedOn
                in zip [MajorLopc, MinorLopc, OtherLopc]
                       (map (tallyBy2 lopcArea1 isOpen (const True)) grouped)
  table ! class_ "list-table-firstcol" $ do
    tr $ do
      th "Classification"
      th "Open/Total"
      th ! colspan (stringValue $ show numAreas) $ "Totals by area"
    forM_ summary $ \ (classification, tallies) -> do
      tr ! Ha.style "height:5em;" $ do
        td $ toHtml (show classification)
        td $ do
          H.span $ toHtml (sum (map second tallies))
          H.span "/"
          H.span $ toHtml (sum (map third tallies))
        let sortByMaxOpen = reverse . sortOn (\ (_, x, _) -> x)
        if null tallies
        then mconcat $ [td "-"] ++ (replicate (numAreas - 1) (td ""))
        else mconcat $ take numAreas $ (flip (++) (repeat (td ""))) $
          (flip map) (sortByMaxOpen tallies) (\ (area, open, closed) -> td $ do
            H.span $ toHtml (show open ++ "/" ++ show closed)
            H.br
            H.span $ toHtml (unpack area))
  p $ toHtml ("There are "
              ++ (show numPastOpen)
              ++ " open LOPC(s) from past years.")

data Status = Open | Closed deriving Eq  -- Hack for selectField
instance Show Status where
  show Open = "open"
  show Closed = "closed"

lopcListPage :: [Entity Lopc] -> Bool -> Html
lopcListPage lopcs isOpen = layout "LOPC List" $ do
  h1 "LOPC"
  lopcNavigation 2
  h2 "LOPC list"
  H.form ! class_ "line-form" ! Ha.style "text-align:left;" $
    selectField "Filter by" "status"
      P.id (Just $ if isOpen then Open else Closed)
      [(Open, "Open"), (Closed, "Closed")]
      ! onchange "this.form.submit()"
  forM_ (groupByAccessor (lopcArea1 . entityVal) lopcs) $ \ lopcs' -> do
    h3 $ toHtml ((lopcArea1 . entityVal) (head lopcs'))
    table ! class_ "list-table" $ do
      col ! class_ "lopc-table-col-1"
      col ! class_ "lopc-table-col-2"
      col ! class_ "lopc-table-col-3"
      col ! class_ "lopc-table-col-4"
      tr $ th "Area" ! colspan "2" >> th "Description" >> th "Reported on"
      forM_ lopcs' $ \ (Entity lid lopc) -> tr $ do
        td $ toHtml (lopcArea2 lopc)
        td $ toHtml (lopcArea3 lopc)
        td $ a ! href (viewLopcLink lid) $ toHtml (lopcDescription lopc)
        td $ toHtml (lopcReportedOn lopc)

lopcPage :: Entity Lopc -> Html
lopcPage (Entity lid lopc) = layout "View LOPC" $ do
  h1 (toHtml ("View LOPC #" ++ (show (fromSqlKey lid))))
  lopcNavigation 2
  lopcDD (Entity lid lopc)

lopcEditPage :: Lopc -> Html
lopcEditPage lopc = layout "Edit LOPC" $ do
  h1 "Edit LOPC"
  lopcNavigation 2
  lopcForm (Just lopc)

lopcNewPage :: Html
lopcNewPage = layout "New LOPC" $ do
  h1 "New LOPC"
  lopcForm Nothing

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
    tr $ th "Pressure" >> td (toHtml $ maybe 0 P.id (lopcPressure lopc))
    tr $ th "Hazardous" >> td (toHtml $ lopcHazardous lopc)
  editableH2 "Misc. info" (toEditLopcLink lid)
  table ! class_ "definition-table" $ do
    tr $ th "Other ref." >> td (toHtml $ lopcOtherRef lopc)
    tr $ th "Remarks" >> td (toHtml $ lopcRemarks lopc)

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
      (maybe P.id
             (\ lopc -> if isJust (lopcClosedOn lopc) then (! Ha.checked "")
                        else P.id)
              mLopc) $
        (input ! Ha.type_ "checkbox" ! Ha.id "isclosed-field" ! Ha.value "Yes")
    let f = if isJust mLopc && isJust (lopcClosedOn (fromJust mLopc))
            then P.id else (! Ha.style "display:none;")
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
      (maybe P.id
             (\ lopc -> if lopcHazardous lopc
                        then (! Ha.checked "")
                        else P.id)
              mLopc) $
        (input ! Ha.type_ "checkbox" ! Ha.name "hazardous" ! Ha.value "Yes")
  fieldset $ do
    legend "Misc. info"
    field "Other ref." "otherref" lopcOtherRef mLopc
    field "Remarks" "remarks" lopcRemarks mLopc
  button "Save"
  when (isJust mLopc)
       (deleteButton "lopc-delete-button" viewLopcOverviewLinkDefaultYear')
  cancelButton "lopc-cancel-button"

lopcNavigation :: Int -> Html
lopcNavigation = navigation
  [ ("Overview", viewLopcOverviewLinkDefaultYear)
  , ("List", viewLopcListLinkDefaultOpen)
  , ("Register new", toCreateLopcLink)
  ]

groupByAccessor :: Ord b => (a -> b) -> [a] -> [[a]]
groupByAccessor f = groupBy ((==) `on` f) . sortOn f

tallyBy2 :: Ord b => (a -> b)
                  -> (a -> Bool)
                  -> (a -> Bool)
                  -> [a]
                  -> [(b, Int, Int)]
tallyBy2 f g1 g2 = map tally . groupByAccessor f
  where tally l = (f (head l), length (filter g1 l), length (filter g2 l))

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z
