{-# LANGUAGE OverloadedStrings #-}

module Common.Views where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as Ha

layout :: String -> Html -> Html
layout pageTitle pageContents = docTypeHtml $ do
  H.head $ do
    meta ! charset "utf-8"
    meta ! httpEquiv "X-UA-Compatible" ! content "IE=Edge"
    H.title (toHtml $ pageTitle ++ " - autotrace")
    script ! src "/underscore.js" $ ""
    script ! src "/jquery.js" $ ""
    script ! src "/jquery-ui.js" $ ""
    script ! src "/d3.js" $ ""
    link ! rel "stylesheet" ! type_ "text/css" ! href "/jquery-ui.css"
    link ! rel "stylesheet" ! type_ "text/css" ! href "/style.css"
  H.body pageContents
