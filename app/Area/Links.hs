{-# LANGUAGE RankNTypes #-}

module Area.Links where

import Servant
import Database.Persist.Postgresql
import Text.Blaze.Html5
import Common.Links
import Area.API
import Schema

viewAreasLink' :: String
viewAreasLink' =
  "/" ++ show (linkTo (Proxy :: Proxy ViewAreas))

viewAreasLink :: AttributeValue
viewAreasLink = stringValue viewAreasLink'

toCreateAreaLink :: Key Area -> AttributeValue
toCreateAreaLink pid = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ToCreateArea) pid)

toCreateTopAreaLink :: AttributeValue
toCreateTopAreaLink = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ToCreateArea'))

viewAreaLink :: Key Area -> AttributeValue
viewAreaLink aid = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ViewArea) aid)
