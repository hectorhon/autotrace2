{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Home.Routes where

import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5

type HomeRoutes = Home

type Home = Get '[HTML] Html
