{-# LANGUAGE OverloadedStrings #-}

module Block.Views where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as Ha
import Database.Persist.Postgresql
import Control.Monad (forM_)
import Common.Views
import Schema
import Time

uploadBlockConfigPage :: UTCTime -> Html
uploadBlockConfigPage uploadDate = layout "Upload block config file" $ do
  h1 "Upload block config file"
  H.form ! method "post" ! enctype "multipart/form-data" $ do
  H.label $ do
    H.span "Snapshot date"
    datepicker "date-field" "snapshotdate" (formatDay uploadDate)
  field "Group" "group" (\ _ -> "" :: String) Nothing
  input ! Ha.name "blockconfig" ! type_ "file"
  button "Save"
  cancelButton "upload-block-config-cancel-button"
