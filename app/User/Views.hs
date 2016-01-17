{-# LANGUAGE OverloadedStrings #-}

module User.Views where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as Ha
import Control.Monad (when)
import Common.Views
import Home.Links
import User.Links

loginPage :: Bool -> Html
loginPage validLogin = layoutNoBanner "Login" $ do
  h1 "Login"
  H.form ! method "post" $ do
    H.label $ do
      H.span "Username"
      input ! Ha.name "name" ! Ha.value ""
    H.label $ do
      H.span "Password"
      input ! Ha.name "password" ! type_ "password" ! Ha.value ""
    H.div ! Ha.style "float:right;" $ do
      when (not validLogin) (H.span $ "Invalid username or password!")
      button "Login"
      a ! Ha.id "login-as-guest-link" ! href "#" $ "Login as guest"
  script $ toHtml $
    " $('#login-as-guest-link').click(function(e) {            \
    \   e.preventDefault();                                  \
    \   $.post('" ++ logoutLink' ++ "').success(function() { \
    \     window.location.href = '" ++ homePageLink' ++ "';   \
    \   });                                                  \
    \ });                                                    "
