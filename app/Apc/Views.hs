{-# LANGUAGE OverloadedStrings #-}

module Apc.Views where

import Text.Blaze.Html5 as H hiding (area, map)
import Text.Blaze.Html5.Attributes as Ha hiding (start)
import Database.Persist.Postgresql
import Control.Monad (forM_)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L (unpack, concat)
import Common.Views
import Area.Links
import Apc.Links
import Schema
import Time

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

apcCalculatePage :: UTCTime -> UTCTime -> Entity Apc -> Html
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
      cancelButton "apc-calculate-cancel-button"

apcPerformancePage :: Entity Apc -> UTCTime -> UTCTime
                   -> [Entity ApcInterval] -> [Entity ApcIssue]
                   -> [Entity Cv] -> [Entity CvInterval]
                   -> Html
apcPerformancePage (Entity aid apc) start end uptimes issues cvs cvExceeds =
  layout "Apc performance" $ do
  h1 $ toHtml (apcName apc)
  apcNavigation (apcArea apc) aid 2
  H.form ! class_ "line-form" ! method "get" $ do
    H.label $ do
      H.span "Start"
      datepicker "start-field" "start" (formatDay start)
    H.label $ do
      H.span "End"
      datepicker "end-field" "end" (formatDay end)
    button "Refresh"
    a "Recalculate"
  h2 ! Ha.style "text-align:center;" $ "Uptime"
  H.div ! Ha.id "summary" $ ""
  timeScale start end
  H.div ! Ha.id "uptimes" $ ""
  H.div ! Ha.id "issues" $ ""
  a "New issue"
  h2 ! Ha.style "text-align:center;" $ "CV constraints"
  timeScale start end
  H.div ! Ha.id "cv-exceeds" $ ""
  a ! href (toCreateApcCvLink (apcArea apc) aid) $ "New CV"
  script $ toHtml $ L.unpack $
    L.concat [ "var start = new Date('", encode start, "');"
             , "var end = new Date('", encode end, "');"
             , "var uptimes = ", (encode uptimes), ";"
             , "var issues = ", (encode issues), ";"
             , "var cvs = ", (encode cvs), ";"
             , "var cvExceeds = ", (encode cvExceeds), ";"   ]
  script ! src "/apc.js" $ ""
  where timeScale start end = table ! class_ "time-scale" $ tr $
          forM_ (splitByHours 6 start end)
                (\ (t, _) -> td $ toHtml $ formatShort t)
        splitByHours pieces start' end' =
          let start = toUnix start'
              end = toUnix end'
              l = start - mod (start + offset*60) 86400
              r = end + (case mod (end + offset*60) 86400 of 0 -> 0
                                                             t -> 86400 - t)
              pieceSize = Prelude.div (r - l) pieces
              startTimes = map (\ n -> l + (n-1)*pieceSize) [1..pieces]
              endTimes = map (\ n -> l + n*pieceSize) [1..pieces]
          in map (\ (s, e) -> (fromUnix $ fromIntegral s,
                               fromUnix $ fromIntegral e )) $
             zip startTimes endTimes
        toUnix t = round $ diffUTCTime t refTime
        fromUnix t = addUTCTime t refTime
        TimeZone offset _ _ = tz

apcNavigation :: Key Area -> Key Apc -> Int -> Html
apcNavigation aid apcId = navigation
  [ ("Definition", viewApcLink aid apcId)
  , ("Calculate", toCalculateApcDefaultDayLink aid apcId)
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
