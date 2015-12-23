{-# LANGUAGE OverloadedStrings #-}

module Area.Views where

import Text.Blaze.Html5 as H hiding (area, link)
import Text.Blaze.Html5.Attributes as Ha
import Database.Persist.Postgresql
import Control.Monad
import Data.Maybe
import Common.Views
import Common.Links
import Area.API
import Servant
import Schema

areaHomePage :: [Entity Area] -> Html
areaHomePage areas = layout "Site list" $ do
  h1 "Select a plant"
  ul $ do
    forM_ areas (\ (Entity aid area) -> let aid' = show (fromSqlKey aid) in
      li $ a ! href (stringValue $ "./" ++ aid' ++ "/definition")
             $ toHtml (areaName area))
    li $ a ! href (stringValue $ show $ linkTo (Proxy :: Proxy ViewAreas))
           $ "New plant..."

areaNewPage :: Maybe (Entity Area) -> Html
areaNewPage mParent = layout "New area" $ do
  h1 "New area"
  areaForm mParent Nothing

areaIdPage :: Entity Area -> Maybe (Entity Area) -> [Entity Area] -> Html
areaIdPage (Entity aid area) mParent children = layout (areaName area) $ do
  H.h1 (toHtml $ areaName area)
  H.h2 "Definition"
  areaForm mParent (Just area)
  H.h2 "Go deeper"
  ul $ do
    forM_ children (\ (Entity aid' area') -> li $
      a ! href (stringValue $  "../" ++ show (fromSqlKey aid') ++ "/definition")
        $ toHtml (areaName area'))
    li $ a ! href (stringValue $  "../new?parent=" ++ show (fromSqlKey aid))
           $ "New subarea..."

areaForm :: Maybe (Entity Area) -> Maybe Area -> Html
areaForm mParent mArea = let
  Entity kParent parent = fromJust mParent
  pid = show (fromSqlKey kParent)
  in H.form ! method "post" $ do
        H.label $ do
          H.span "Name"
          input ! Ha.name "name"
                ! Ha.value (stringValue (maybe "" areaName mArea))
        H.label $ do
          H.span "Description"
          input ! Ha.name "description"
                ! Ha.value (stringValue (maybe "" areaDescription mArea))
        when (isJust mParent) (do
          H.label $ do
            H.span "Parent"
            let link = if isJust mArea then "../" ++ pid ++ "/definition"
                       else pid ++ "/definition"
            a ! class_ "input-link"
              ! href (stringValue link) $ toHtml (areaName parent)
          input ! Ha.name "parent"
                ! Ha.type_ "hidden"
                ! Ha.value (stringValue pid))
        button "Save"
        if isJust mArea then do
          button ! Ha.id "area-delete-button" $ "Delete"
          script " $('#area-delete-button').click(function(e) { \
                 \   e.preventDefault(); \
                 \   $.ajax({ \
                 \     method: 'DELETE', \
                 \     url: window.location, \
                 \     success: function() { \
                 \       window.location.replace('../../area'); \
                 \     } \
                 \   }) \
                 \ }); "
        else do
          button ! Ha.id "area-cancel-button" $ "Cancel"
          script " $('#area-cancel-button').click(function(e) { \
                 \   e.preventDefault(); \
                 \   window.location.href = document.referrer; \
                 \ }); "
