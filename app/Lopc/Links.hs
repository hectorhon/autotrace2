module Lopc.Links where

import Servant
import Database.Persist.Postgresql
import Text.Blaze.Html5
import Common.Links
import Lopc.Types
import Lopc.Routes

viewLopcsLinkDefaultYear :: AttributeValue
viewLopcsLinkDefaultYear = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ViewLopcs'))



viewLopcsOverviewLink :: Integer -> AttributeValue
viewLopcsOverviewLink year = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ViewLopcsOverview) year)

viewLopcsOverviewDefaultYearLink' :: String
viewLopcsOverviewDefaultYearLink' =
  "/" ++ show (linkTo (Proxy :: Proxy ViewLopcsOverview'))

viewLopcsOverviewDefaultYearLink :: AttributeValue
viewLopcsOverviewDefaultYearLink = stringValue viewLopcsOverviewDefaultYearLink'



viewLopcLink' :: Key Lopc -> String
viewLopcLink' lid =
  "/" ++ show (linkTo (Proxy :: Proxy ViewLopc) lid)

viewLopcLink :: Key Lopc -> AttributeValue
viewLopcLink lid = stringValue (viewLopcLink' lid)



toEditLopcLink' :: Key Lopc -> String
toEditLopcLink' lid =
  "/" ++ show (linkTo (Proxy :: Proxy ToEditLopc) lid)

toEditLopcLink :: Key Lopc -> AttributeValue
toEditLopcLink lid = stringValue (toEditLopcLink' lid)
