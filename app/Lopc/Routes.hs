{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lopc.Routes where

import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5

type LopcRoutes = ViewLopcs

type ViewLopcs = "lopc" :> "summary" :> QueryParam "year" Integer
                 :> Get '[HTML] Html
