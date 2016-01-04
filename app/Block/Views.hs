{-# LANGUAGE OverloadedStrings #-}

module Block.Views where

import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as Ha
import Database.Persist.Postgresql
import Control.Monad (forM_)
import Common.Views
import Block.Links
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

searchBlocksPage :: Maybe String -> [Entity BlockHead] -> Html
searchBlocksPage mSearchStr blocks = layout "Search blocks" $ do
  h1 "Search for blocks"
  H.form $ do
    field "Name" "pname" Prelude.id mSearchStr
    button "Search"
  case mSearchStr of
    Nothing -> return ()
    Just searchStr -> do
      h2 "Search results"
      if null blocks then p (toHtml $ "No blocks found for text " ++ searchStr)
      else table ! class_ "list-table" $ do
        tr $ do
          th "Name"
          th "Type"
          th "Last local database update"
          th "Group"
        forM_ blocks $ \ (Entity bid (BlockHead blockName t _ date group)) ->
          tr $ do
            td $ a ! href (viewBlockLink bid) $ toHtml blockName
            td $ toHtml t
            td $ toHtml $ formatDay date
            td $ toHtml group

viewBlockPage :: BlockHead -> [Entity BlockAttr] -> Html
viewBlockPage block attrs = layout "Block detail" $ do
  h1 (toHtml $ blockHeadName block)
  p (toHtml $ "Registered on: " ++ (formatDay $ blockHeadFirstDate block))
  p (toHtml $ "Last local database update: "
              ++ (formatDay $ blockHeadCurrentDate block))
  p (toHtml $ "Group: " ++ (blockHeadGroup block))
  table ! class_ "list-table" $ do
    tr $ th "Parameter" >> th "Value"
    tr $ td "NAME" >> td (toHtml $ blockHeadName block)
    tr $ td "TYPE" >> td (toHtml $ blockHeadType_ block)
    forM_ (map entityVal attrs) $ \ (BlockAttr _ _ key val) ->
      tr $ td (toHtml key) >> td (toHtml val)
