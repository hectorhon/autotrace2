{-# LANGUAGE OverloadedStrings #-}

module Home.Handlers where

import Servant
import Text.Blaze.Html5
import AppM
import Home.Routes
import Home.Views

homeHandlers :: ServerT HomeRoutes AppM
homeHandlers = home

home :: AppM Html
home = return homePage
