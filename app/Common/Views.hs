{-# LANGUAGE OverloadedStrings #-}

module Common.Views where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as Ha
import Control.Monad (forM_)

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

field :: Show b => String -> String -> (a -> b) -> Maybe a -> Html
field fieldLabel fieldName accessor mRecord = H.label $ do
  H.span (toHtml fieldLabel)
  input ! Ha.name (stringValue fieldName)
        ! Ha.value (stringValue $ maybe "" (show . accessor) mRecord)

linkField :: String -> String -> AttributeValue-> Html
linkField fieldLabel displayText url = H.label $ do
  H.span (toHtml fieldLabel)
  a ! class_ "input-link" ! href url $ toHtml displayText

hiddenField :: String -> String -> Html
hiddenField fieldName val =
  input ! Ha.name (stringValue fieldName)
        ! type_ "hidden"
        ! Ha.value (stringValue val)

selectField :: (Eq b, Show b)
            => String -> String -> (a -> b) -> Maybe a -> [(b, String)]
            -> Html
selectField fieldLabel fieldName accessor mRecord options = H.label $ do
  H.span (toHtml fieldLabel)
  let f sel = if maybe False (((==) sel) . accessor) mRecord
              then (! Ha.selected "selected") else (Prelude.id)
  select ! Ha.name (stringValue fieldName) $ do
    forM_ options (\ (choice, optionLabel) ->
      f choice (H.option ! Ha.value (stringValue $ show choice)
                         $ toHtml optionLabel))

cancelButton :: String -> Html
cancelButton buttonId = do
  button ! Ha.id (stringValue buttonId) $ "Cancel"
  script $ toHtml $
    " $('#"++buttonId++"').click(function(e) { \
    \   e.preventDefault(); \
    \   window.location.href = document.referrer; \
    \ }); "

deleteButton :: String -> String -> Html
deleteButton buttonId url = do
  button ! Ha.id (stringValue buttonId) $ "Delete"
  script $ toHtml $
    " $('#"++buttonId++"').click(function(e) { \
    \   e.preventDefault(); \
    \   $.ajax({ \
    \     method: 'DELETE', \
    \     url: window.location, \
    \     success: function() { \
    \       window.location.replace('"++ url ++"'); \
    \     } \
    \   }) \
    \ }); "
