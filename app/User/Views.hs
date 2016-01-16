{-# LANGUAGE OverloadedStrings #-}

module User.Views where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as Ha
import Common.Views

loginPage :: Html
loginPage = layout "Login" $ do
  H.form ! method "post" $ do
    field "Username" "name" (\ _ -> "") Nothing
    field "Password" "password" (\ _ -> "") Nothing
    button "Login"
