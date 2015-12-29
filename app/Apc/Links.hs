{-# LANGUAGE RankNTypes #-}

module Apc.Links where

import Servant
import Database.Persist.Postgresql
import Text.Blaze.Html5
import Common.Links
import Apc.API
import Schema

toCreateApcLink :: Key Area -> AttributeValue
toCreateApcLink aid = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ToCreateApc) aid)



viewApcLink' :: Key Area -> Key Apc -> String
viewApcLink' aid bid =
  "/" ++ show (linkTo (Proxy :: Proxy ViewApc) aid bid)

viewApcLink :: Key Area -> Key Apc -> AttributeValue
viewApcLink aid bid = stringValue $ viewApcLink' aid bid
