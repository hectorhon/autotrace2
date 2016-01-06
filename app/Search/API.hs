{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Search.API where

import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5

type SearchSite = Search

type Search = "search"
           :> QueryParam "pname" String
           :> Get '[HTML] Html
