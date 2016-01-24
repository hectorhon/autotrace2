{-# LANGUAGE RankNTypes #-}

module Area.Links where

import Servant
import Database.Persist.Postgresql
import Text.Blaze.Html5
import Common.Links
import Area.API
import Area.Types

toCreateAreaLink :: Key Area -> AttributeValue
toCreateAreaLink pid = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ToCreateArea) pid)

toCreateTopAreaLink :: AttributeValue
toCreateTopAreaLink = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ToCreateArea'))



viewAreasLink' :: String -> String
viewAreasLink' target =
  "/" ++ show (linkTo (Proxy :: Proxy ViewAreas) target)

viewAreasLink :: String -> AttributeValue
viewAreasLink = stringValue . viewAreasLink'



viewAreaLink' :: Key Area -> String
viewAreaLink' aid =
  "/" ++ show (linkTo (Proxy :: Proxy ViewArea) aid)

viewAreaLink :: Key Area -> AttributeValue
viewAreaLink = stringValue . viewAreaLink'



toEditAreaLink :: Key Area -> AttributeValue
toEditAreaLink aid = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ToEditArea) aid)
