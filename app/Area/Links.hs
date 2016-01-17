{-# LANGUAGE RankNTypes #-}

module Area.Links where

import Servant
import Database.Persist.Postgresql
import Text.Blaze.Html5
import Common.Links
import Area.API
import Schema

toCreateAreaLink :: Key Area -> AttributeValue
toCreateAreaLink pid = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ToCreateArea) pid)

toCreateTopAreaLink :: AttributeValue
toCreateTopAreaLink = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ToCreateArea'))



viewAreasLink' :: String
viewAreasLink' =
  "/" ++ show (linkTo (Proxy :: Proxy ViewAreas))

viewAreasLink :: AttributeValue
viewAreasLink = stringValue viewAreasLink'



viewAreaLink' :: Key Area -> String
viewAreaLink' aid =
  "/" ++ show (linkTo (Proxy :: Proxy ViewArea) aid)

viewAreaLink :: Key Area -> AttributeValue
viewAreaLink = stringValue . viewAreaLink'



toEditAreaLink :: Key Area -> AttributeValue
toEditAreaLink aid = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ToEditArea) aid)
