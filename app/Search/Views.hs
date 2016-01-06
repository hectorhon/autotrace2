{-# LANGUAGE OverloadedStrings #-}

module Search.Views where

import Text.Blaze.Html5 as H hiding (link)
import Text.Blaze.Html5.Attributes as Ha hiding (name)
import Control.Monad (forM_)
import Common.Views
import Search.Types

searchPage :: Maybe String -> [Searchable] -> Html
searchPage mpname results = layout "Search" $ do
  h1 "Search"
  H.form ! class_ "line-form" $ do
    field "" "pname" Prelude.id mpname
    button "Search"
  case length results of
    1 -> p "1 result found."
    n -> p $ toHtml (show n ++ " results found.")
  table ! class_ "list-table" $ do
    tr $ th "Name" >> th "Description" >> th "Type"
    forM_ results $ \ result -> do
      td $ a ! href (link result) $ toHtml (name result)
      td $ toHtml $ description result
      td $ toHtml $ category result
