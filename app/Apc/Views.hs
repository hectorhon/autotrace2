{-# LANGUAGE OverloadedStrings #-}

module Apc.Views where

import Text.Blaze.Html5 as H hiding (area, map, head)
import Text.Blaze.Html5.Attributes as Ha hiding (start)
import Database.Persist.Postgresql
import Control.Monad (forM_)
import Data.Aeson
import Data.HashMap.Strict (fromList)
import Data.List (groupBy, sortOn)
import Data.Function (on)
import qualified Data.ByteString.Lazy.Char8 as L (unpack, concat)
import Common.Views
import Area.Types
import Area.Links
import Apc.Links
import Apc.Issue.Links
import Schema
import Time
import TimeSeriesData

apcNewPage :: Entity Area -> Html
apcNewPage parent = layout "New advanced process controller" $ do
  h1 "New APC"
  apcForm parent Nothing

apcIdPage :: Entity Apc -> Entity Area -> [Entity Cv] -> Html
apcIdPage (Entity aid apc) parent cvs = layout (apcName apc) $ do
  h1 $ toHtml (apcName apc)
  apcNavigation (entityKey parent) aid 1
  editableH2 "Definition" (toEditApcLink (entityKey parent) aid)
  apcDD apc parent
  h2 "CVs"
  ul $ do
    forM_ cvs (\ (Entity cid cv) -> li $
      a ! href (viewApcCvLink (apcArea apc) aid cid) $ toHtml (cvName cv))
    li $ a ! href (toCreateApcCvLink (apcArea apc) aid) $ "New CV..."

apcEditPage :: Entity Apc -> Entity Area -> Html
apcEditPage (Entity aid apc) parent = layout (apcName apc) $ do
  h1 $ toHtml (apcName apc)
  apcNavigation (entityKey parent) aid 1
  h2 "Definition - edit"
  apcForm parent (Just apc)

apcsPage :: [Entity Apc] -> [[Entity ApcInterval]] -> UTCTime -> UTCTime -> Html
apcsPage apcs uptimess start' end' = layout "APC list" $ do
  h1 "APC list"
  table ! class_ "apc-list-table" $ do
    tr $ th "" >> th "Uptime today"
    forM_ (groupByAccessor (head . apcName . entityVal) apcs) $ \ apcs' -> do
      tr $ td ! colspan "2"
              ! Ha.style "text-align:center;height:1em;"
              $ ""
      forM_ apcs' (\ (Entity aid apc) -> tr $ do
        td ! Ha.style "text-align:center;"
           $ a ! href (viewApcPerformanceDefaultDayLink (apcArea apc) aid)
               $ toHtml (apcName apc)
        td ! Ha.id (stringValue ("uptimes-" ++ show (fromSqlKey aid)))
          ! Ha.style "position:relative;background-color:lightgrey;width:500px;"
          $ "")
  p $ do
    H.span "To create a new APC, select the specific area "
    a ! href (viewAreasLink "area") $ "here"
    H.span "."
  script $ toHtml $ L.unpack $
    L.concat [ "var start = new Date('", encode start', "');"
             , "var end = new Date('", encode end', "');"
             , "var uptimess = ", (encode uptimess), ";"        ]
  script ! src "/apcs.js" $ ""

groupByAccessor :: Ord b => (a -> b) -> [a] -> [[a]]
groupByAccessor f = groupBy ((==) `on` f) . sortOn f

apcCalculatePage :: Day -> Day -> Entity Apc -> Html
apcCalculatePage start end (Entity aid apc) =
  layout "APC performance calculation" $ do
    h1 $ toHtml (apcName apc)
    apcNavigation (apcArea apc) aid 2
    H.form ! method "post" $ do
      H.label $ do
        H.span "Start"
        datepicker "start-field" "start" (formatDay start)
      H.label $ do
        H.span "End"
        datepicker "end-field" "end" (formatDay end)
      button "Submit"

apcPerformancePage :: Entity Apc -> Day -> Day
                   -> [Entity ApcInterval] -> [Entity ApcIssue]
                   -> [Entity Cv] -> [Entity CvInterval]
                   -> Html
apcPerformancePage (Entity aid apc) start end uptimes issues cvs cvExceeds =
  let start' = localDayToUTC start
      end' = localDayToUTC (addDays 1 end)
  in layout "Apc performance" $ do
    h1 $ toHtml (apcName apc)
    apcNavigation (apcArea apc) aid 3
    H.form ! class_ "line-form" ! method "get" $ do
      H.label $ do
        H.span "Start"
        datepicker "start-field" "start" (formatDay start)
      H.label $ do
        H.span "End"
        datepicker "end-field" "end" (formatDay end)
      button "Refresh"
      a ! href (toCalculateApcLink (apcArea apc) aid start end)
        $ "Recalculate"
    h2 ! Ha.style "text-align:center;" $ "Uptime"
    H.div ! Ha.id "summary" $ ""
    timeScale start' end'
    H.div ! Ha.id "uptimes" $ ""
    H.div ! Ha.id "issues" $ ""
    h2 ! Ha.style "text-align:center;" $ "CV constraints"
    h3 ! Ha.style "text-align:center;" $ "Economic"
    timeScale start' end'
    H.div ! Ha.id "cv-exceeds-economic" $ ""
    h3 ! Ha.style "text-align:center;" $ "Constraint"
    timeScale start' end'
    H.div ! Ha.id "cv-exceeds-constraint" $ ""
    h3 ! Ha.style "text-align:center;" $ "Protective"
    timeScale start' end'
    H.div ! Ha.id "cv-exceeds-protective" $ ""
    script $ toHtml $ L.unpack $
      L.concat [ "var start = new Date('", encode start', "');"
               , "var end = new Date('", encode end', "');"
               , "var uptimes = ", (encode uptimes), ";"
               , "var issues = ", (encode issues), ";"
               , "var cvs = ", (encode cvs), ";"
               , "var cvExceeds = ", (encode cvExceeds), ";"   ]
    script ! src "/apc.js" $ ""
    where timeScale s e =
            table ! class_ "time-scale" $ tr $
              forM_ (splitByHours 6 s e)
                    (\ (t, _) -> td $ toHtml $ formatShort t)
          splitByHours pieces s e =
            let l = (toUnix s) - mod ((toUnix s) + offset*60) 86400
                t = mod ((toUnix e) + offset*60) 86400
                r = (toUnix e) + (if t /= 0 then 86400 - t else 0)
                pieceSize = Prelude.div (r - l) pieces
                startTimes = map (\ n -> l + (n-1)*pieceSize) [1..pieces]
                endTimes = map (\ n -> l + n*pieceSize) [1..pieces]
            in map (\ (s', e') -> (fromUnix $ fromIntegral s',
                                   fromUnix $ fromIntegral e' )) $
               zip startTimes endTimes
          toUnix t = round $ diffUTCTime t refTime
          fromUnix t = addUTCTime t refTime
          TimeZone offset _ _ = tz

apcNavigation :: Key Area -> Key Apc -> Int -> Html
apcNavigation aid apcId = navigation
  [ ("Definition", viewApcLink aid apcId)
  , ("Calculate", toCalculateApcDefaultDayLink aid apcId)
  , ("Performance", viewApcPerformanceDefaultDayLink aid apcId)
  , ("Issues", viewApcIssuesLink aid apcId)
  ]

apcCvNewPage :: Entity Apc -> Html
apcCvNewPage apc = layout "New CV" $ do
  h1 "New CV"
  apcCvForm apc Nothing

apcCvIdPage :: Entity Cv -> Entity Apc -> Html
apcCvIdPage (Entity cid cv) apc = layout (cvName cv) $ do
  h1 $ toHtml (cvName cv)
  cvNavigation (apcArea $ entityVal apc) (entityKey apc) cid 1
  editableH2 "Definition" $
    toEditApcCvLink (apcArea $ entityVal apc) (cvApc cv) cid
  apcCvDD cv apc

apcCvEditPage :: Entity Cv -> Entity Apc -> Html
apcCvEditPage (Entity cid cv) apc = layout (cvName cv) $ do
  h1 $ toHtml (cvName cv)
  cvNavigation (apcArea $ entityVal apc) (entityKey apc) cid 1
  h2 "Definition - edit"
  apcCvForm apc (Just cv)

apcCvTrendPage :: Entity Cv -> Entity Apc -> TSData -> Html
apcCvTrendPage (Entity cid cv) (Entity aid apc) (TSData startN endN tsData) =
  layout (cvName cv) $ do
    h1 $ toHtml (cvName cv)
    cvNavigation (apcArea apc) aid cid 2
    H.div ! Ha.id "chart" $ ""
    let startDay = utcToLocalDay $ addUTCTime startN refTime
    let endDay = utcToLocalDay $ addUTCTime endN refTime
    a ! href (viewApcPerformanceLink (apcArea apc) aid startDay endDay)
      $ "Back to APC performance"
    script $ toHtml $
      "var data = " ++ (L.unpack $ encode (fromList tsData)) ++ ";"
    script $ toHtml $ "var start = "
                      ++ (L.unpack $ encode (realToFrac startN :: Double))
                      ++ ";"
    script $ toHtml $ "var end = "
                      ++ (L.unpack $ encode (realToFrac endN :: Double))
                      ++ ";"
    script ! src "/cvChart.js" $ ""

cvNavigation :: Key Area -> Key Apc -> Key Cv -> Int -> Html
cvNavigation aid apcId cid = navigation
  [ ("Definition", viewApcCvLink aid apcId cid)
  , ("Trend", viewApcCvTrendDefaultDayLink aid apcId cid)
  ]

apcForm :: Entity Area -> Maybe Apc -> Html
apcForm (Entity pid parent) mApc = H.form ! method "post" $ do
  field       "Name"                  "name"        apcName          mApc
  field       "Uptime condition"      "uptimecond"  apcUptimeCond    mApc
  linkField   "Parent" (areaName parent) (viewAreaLink pid)
  hiddenField "area" (show $ fromSqlKey pid)
  button "Save"
  maybe (return ())
        (deleteButton "apc-delete-button" . viewAreaLink' . apcArea) mApc
  cancelButton "apc-cancel-button"

apcDD :: Apc -> Entity Area -> Html
apcDD apc (Entity pid parent) = table ! class_ "definition-table" $ do
  tr $ th "Name" >> td (toHtml $ apcName apc)
  tr $ th "Uptime condition" >> td (toHtml $ apcUptimeCond apc)
  tr $ do
    th "Parent"
    td $ a ! href (viewAreaLink pid) $ (toHtml $ areaName parent)

apcCvForm :: Entity Apc -> Maybe Cv -> Html
apcCvForm (Entity aid apc) mCv = H.form ! method "post" $ do
  field "Name"               "name"    cvName    mCv
  linkField "APC" (apcName apc) (viewApcLink (apcArea apc) aid)
  hiddenField "area" (show $ fromSqlKey aid)
  selectField "Category" "category" cvCategory mCv
    [ (EconomicCv  , "Economic"  )
    , (ConstraintCv, "Constraint")
    , (ProtectiveCv, "Protective")
    ]
  field "Measurement tag"    "meastag" cvMeasTag mCv
  field "Set range high tag" "srhtag"  cvSrhTag  mCv
  field "Set range low tag"  "srltag"  cvSrlTag  mCv
  field "Prediction tag"     "predtag" cvPredTag mCv
  field "Selected tag"       "seltag"  cvSelTag  mCv
  button "Save"
  maybe (return ())
        (deleteButton "apc-cv-delete-button"
        . viewApcLink' (apcArea apc) . cvApc) mCv
  cancelButton "cv-cancel-button"

apcCvDD :: Cv -> Entity Apc -> Html
apcCvDD cv (Entity aid apc) = table ! class_ "definition-table" $ do
  tr $ th "Name" >> td (toHtml $ cvName cv)
  tr $ do
    th "APC"
    td $ a ! href (viewApcLink (apcArea apc) aid) $ (toHtml $ apcName apc)
  tr $ th "Category"           >> td (toHtml $ show $ cvCategory cv)
  tr $ th "Measurement tag"    >> td (toHtml $ cvMeasTag cv)
  tr $ th "Set range high tag" >> td (toHtml $ cvSrhTag cv)
  tr $ th "Set range low tag"  >> td (toHtml $ cvSrlTag cv)
  tr $ th "Prediction tag"     >> td (toHtml $ cvPredTag cv)
  tr $ th "Selected tag"       >> td (toHtml $ cvSelTag cv)
