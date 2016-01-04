{-# LANGUAGE RankNTypes #-}

module Block.Links where

import Servant
import Database.Persist.Postgresql
import Text.Blaze.Html5
import Common.Links
import Block.API
import Schema

viewBlockLink :: Key BlockHead -> AttributeValue
viewBlockLink bid = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ViewBlock) bid)
