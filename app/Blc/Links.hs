{-# LANGUAGE RankNTypes #-}

module Blc.Links where

import Servant
import Database.Persist.Postgresql
import Text.Blaze.Html5
import Common.Links
import Blc.API
import Schema

toCreateBlcLink :: Key Area -> AttributeValue
toCreateBlcLink aid = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ToCreateBlc) aid)



viewBlcLink' :: Key Area -> Key Blc -> String
viewBlcLink' aid bid =
  "/" ++ show (linkTo (Proxy :: Proxy ViewBlc) aid bid)

viewBlcLink :: Key Area -> Key Blc -> AttributeValue
viewBlcLink aid bid = stringValue $ viewBlcLink' aid bid
