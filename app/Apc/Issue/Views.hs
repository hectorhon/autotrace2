{-# LANGUAGE OverloadedStrings #-}

module Apc.Issue.Views where

import Text.Blaze.Html5 as H hiding (area)
import Text.Blaze.Html5.Attributes as Ha hiding (start)
import Database.Persist.Postgresql
import Control.Monad (forM_)
import Data.Aeson
import Data.ByteString.Lazy.Char8 (unpack)
import Common.Views
import Apc.Issue.Links
import Apc.Links
import Apc.Views (apcNavigation)
import Schema
import Time

apcIssueNewPage :: Entity Apc -> UTCTime -> UTCTime -> [String] -> Html
apcIssueNewPage apc start end categories = layout "New Issue" $ do
  h1 "New Issue"
  apcIssueForm apc (Left (start, end)) categories

apcIssueIdPage :: Entity ApcIssue -> Entity Apc -> [String] -> Html
apcIssueIdPage (Entity iid issue) apc categories = layout "APC issues" $ do
  h1 $ toHtml ("Issue #" ++ (show $ fromSqlKey iid))
  apcIssueNavigation (apcArea $ entityVal apc) (entityKey apc) iid 1
  apcIssueForm apc (Right issue) categories

apcIssuesPage :: [Entity ApcIssue] -> Entity Apc -> Html
apcIssuesPage issues (Entity aid apc) = layout "APC issues" $ do
  h1 $ toHtml (apcName apc)
  apcNavigation (apcArea apc) aid 4
  table ! class_ "issues-table" $ do
    col ! class_ "issues-table-col-1"
    col ! class_ "issues-table-col-2"
    col ! class_ "issues-table-col-3"
    col ! class_ "issues-table-col-4"
    col ! class_ "issues-table-col-5"
    col ! class_ "issues-table-col-6"
    tr $ do
      th "#"
      th "Day"
      th "Duration"
      th "Category"
      th "Description"
      th "Edit"
    forM_ issues (\ (Entity iid issue) -> tr $ do
      td $ toHtml (show $ fromSqlKey iid)
      td $ toHtml (formatDay $ apcIssueStart issue)
      let duration = roundTo 1
            ((diffUTCTime (apcIssueEnd issue) (apcIssueStart issue)) / 3600)
      td $ toHtml $ show duration
      td $ toHtml (apcIssueCategory issue)
      let description' = flip (++) "..." $ trim 20 (apcIssueDescription issue)
      td $ toHtml description'
      td $ (a ! href (viewApcIssueLink (apcArea apc) aid iid) $ "Edit"))
  p $ a ! href (toCreateApcIssueLink (apcArea apc) aid) $ "New issue..."

roundTo :: RealFrac a => Int -> a -> Double
roundTo places x = (realToFrac (round (x' * h) :: Int) :: Double) / h
  where x' = realToFrac x
        h = 10 ^ places

trim :: Int -> String -> String
trim n = unwords . take n . words

apcIssueNavigation :: Key Area -> Key Apc -> Key ApcIssue -> Int -> Html
apcIssueNavigation aid apcId iid = navigation
  [ ("Overview", viewApcIssueLink aid apcId iid)
  ]

apcIssueForm :: Entity Apc -> Either (UTCTime, UTCTime) ApcIssue -> [String]
             -> Html
apcIssueForm (Entity aid apc) mIssue categories = H.form ! method "post" $ do
  linkField "APC" (apcName apc) (viewApcLink (apcArea apc) aid)
  hiddenField "apc" (show $ fromSqlKey aid)
  H.label $ do
    H.span "Start"
    datepicker "start-field" "startday"
      (formatDay $ either fst apcIssueStart mIssue)
    input ! Ha.id "starttime-field"
          ! Ha.name "starttime"
          ! Ha.value (stringValue $ either (\ _ -> "00:00")
                                           (formatClock . apcIssueStart)
                                           mIssue)
  H.label $ do
    H.span "End"
    datepicker "end-field" "endday"
      (formatDay $ either fst apcIssueEnd mIssue)
    input ! Ha.id "endtime-field"
          ! Ha.name "endtime"
          ! Ha.value (stringValue $ either (\ _ -> "00:00")
                                           (formatClock . apcIssueEnd)
                                           mIssue)
  field' "Category" "category" (Just "category-field") Nothing
    apcIssueCategory (eitherToMaybe mIssue)
  largeField' "Description" "description" Nothing Nothing
    apcIssueDescription (eitherToMaybe mIssue)
  script $ toHtml $ " $('#category-field').autocomplete({ \
                    \    source: " ++ unpack (encode categories) ++
                    "   , minLength: 0 \
                    \ }).focus(function() { \
                    \   $('#category-field').autocomplete('search', '') \
                    \ }); "
  H.label $ do
    H.span "Discount uptime?"
    (either (\ _ -> Prelude.id)
            (\ issue -> if apcIssueDiscountUptime issue
                        then (! Ha.checked "")
                        else Prelude.id)
            mIssue) $
      (input ! Ha.type_ "checkbox"
             ! Ha.name "discountuptime"
             ! Ha.value "Yes")
  button "Save"
  case mIssue of
    Right issue -> (deleteButton "apc-issue-delete-button"
                    . viewApcIssuesLink' (apcArea apc) . apcIssueApc) issue
    _           -> return ()
  cancelButton "apc-issue-cancel-button"

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (\ _ -> Nothing) Just
