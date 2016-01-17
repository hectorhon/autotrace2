{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}

module Common.Views where

import Text.Blaze.Html5 as H hiding (i)
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
  H.body $ do
    banner
    pageContents

banner :: Html
banner = H.div ! class_ "banner" $ do
  a ! href "/" $ "autotrace"
  H.span ! Ha.style "float:right;" $ do
    H.span ! Ha.style "margin:0 10px;" $ do
      H.span "Logged in as "
      H.span $ H.b ! Ha.id "username-field" $ ""
    a ! href "#" ! Ha.id "login-logout-link" $ ""
  script " var cookies = {};                                 \
         \ document.cookie.split('; ').forEach(function(c) { \
         \   var t = c.split('=');                           \
         \   cookies[t[0]]=t[1];                             \
         \ });                                               \
         \ if (!cookies['username']) {                       \
         \   $('#username-field').text('guest');             \
         \   $('#login-logout-link').text('Change user...'); \
         \ } else if (cookies['username'] == 'guest') {      \
         \   $('#username-field').text('guest');             \
         \   $('#login-logout-link').text('Change user...'); \
         \ } else {                                          \
         \   $('#username-field').text(cookies['username']); \
         \   $('#login-logout-link').text('Logout');         \
         \ }                                                 "
  script " $('#login-logout-link').click(function(e) {       \
         \   e.preventDefault();                             \
         \   $.post('/logout').success(function() {          \
         \     window.location.href = '/login';              \
         \   });                                             \
         \ });                                               "

class ToString a where
  toString :: a -> String
instance ToString String where
  toString = Prelude.id
instance Show a => ToString a where
  toString = show

field :: ToString b => String -> String -> (a -> b) -> Maybe a -> Html
field fieldLabel fieldName accessor mRecord = H.label $ do
  H.span (toHtml fieldLabel)
  input ! Ha.name (stringValue fieldName)
        ! Ha.value (stringValue $ maybe "" (toString . accessor) mRecord)

-- |The field' function additionally accepts id and class
field' :: ToString b
       => String -> String -> Maybe String -> Maybe String
       -> (a -> b) -> Maybe a -> Html
field' fieldLabel fieldName mFieldId mFieldClass accessor mRecord = H.label $ do
  H.span (toHtml fieldLabel)
  let f = case mFieldId of
            Nothing -> Prelude.id
            Just fieldId -> \ i -> i ! Ha.id (stringValue fieldId)
  let g = case mFieldClass of
            Nothing -> Prelude.id
            Just fieldClass -> \ i -> i ! Ha.class_ (stringValue fieldClass)
  f . g $
    input ! Ha.name (stringValue fieldName)
          ! Ha.value (stringValue $ maybe "" (toString . accessor) mRecord)

-- |The largeField' function is a textarea and additionally accepts id and class
largeField' :: ToString b
            => String -> String -> Maybe String -> Maybe String
            -> (a -> b) -> Maybe a -> Html
largeField' fieldLabel fieldName mFieldId mFieldClass accessor mRecord =
  H.label $ do
    H.span (toHtml fieldLabel)
    let f = case mFieldId of
              Nothing -> Prelude.id
              Just fieldId -> \ i -> i ! Ha.id (stringValue fieldId)
    let g = case mFieldClass of
              Nothing -> Prelude.id
              Just fieldClass -> \ i -> i ! Ha.class_ (stringValue fieldClass)
    f . g $
      textarea ! Ha.name (stringValue fieldName)
               ! Ha.rows "4"
               $ toHtml $ maybe "" (toString . accessor) mRecord

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
    " $('#"++buttonId++"').click(function(e) {                \
    \   e.preventDefault();                                   \
    \   if (confirm('Permanently delete this item?')) {       \
    \     $.ajax({                                            \
    \       method: 'DELETE',                                 \
    \       url: window.location,                             \
    \       success: function() {                             \
    \         window.location.replace('"++ url ++"');         \
    \       }                                                 \
    \     })                                                  \
    \   }                                                     \
    \ });                                                     "

datepicker :: String -> String -> String -> Html
datepicker fieldId fieldName fieldValue = do
  input ! Ha.id (stringValue fieldId)
        ! Ha.name (stringValue fieldName)
        ! Ha.value (stringValue fieldValue)
  script $ toHtml $
    " $('#" ++ fieldId ++ "').datepicker({ \
    \   dateFormat: 'dd . mm . yy', \
    \   maxDate: 0, \
    \ });"

navigation :: [(String, AttributeValue)] -> Int -> Html
navigation links choice = ul ! class_ "navigation" $ forM_ (zip [1..] links)
  (\ (index, (linkLabel, linkUrl)) -> li $
    a ! href linkUrl
      ! class_ (if choice == index then "selected" else "")
      $ toHtml linkLabel)

bar :: Real a => String -> a -> a -> String -> Html
bar color val maxVal barContents = let
  val'      = realToFrac val :: Double
  maxVal'   = realToFrac maxVal :: Double
  percent   = if maxVal' == 0 then 0 else val' / maxVal' * 100
  tooltip   = show (round percent :: Int) ++ " %"
  barContents' = if null barContents then tooltip else barContents
  in H.div ! class_ "bar"
           ! Ha.style (stringValue $ "border:1px solid " ++ color ++";")
           ! Ha.title (stringValue tooltip) $ do
       H.div ! class_ "bar-inner"
             ! Ha.style (stringValue $
                         "background-color:" ++ color        ++ ";"
                      ++ "width:"            ++ show percent ++ "%;")
             $ ""
       H.div ! class_ "bar-inner-text" $ toHtml barContents'

roundTo :: RealFrac a => Int -> a -> Double
roundTo places x = (realToFrac (round (x' * h) :: Int) :: Double) / h
  where x' = realToFrac x
        h = 10 ^ places
