{-# LANGUAGE OverloadedStrings #-}

module Home.Views where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as Ha
import Common.Views
import Area.Links
import Apc.Links

homePage :: Html
homePage = layout "Home" $ do
  p $ a ! href (viewAreasLink "blc") $ "Browse base layer controllers"
  p $ a ! href viewApcsLink $ "Browse APCs"
