module Lopc.Links where

import Servant
import Database.Persist.Postgresql
import Text.Blaze.Html5
import Common.Links
import Lopc.Types
import Lopc.Routes

viewLopcOverviewLinkDefaultYear' :: String
viewLopcOverviewLinkDefaultYear' =
  "/" ++ show (linkTo (Proxy :: Proxy ViewLopcOverview'))

viewLopcOverviewLinkDefaultYear :: AttributeValue
viewLopcOverviewLinkDefaultYear = stringValue viewLopcOverviewLinkDefaultYear'

viewLopcListLinkDefaultOpen :: AttributeValue
viewLopcListLinkDefaultOpen = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ViewLopcList'))

viewLopcLink' :: Key Lopc -> String
viewLopcLink' lid =
  "/" ++ show (linkTo (Proxy :: Proxy ViewLopc) lid)

viewLopcLink :: Key Lopc -> AttributeValue
viewLopcLink lid = stringValue (viewLopcLink' lid)

toCreateLopcLink :: AttributeValue
toCreateLopcLink = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ToCreateLopc))

toEditLopcLink' :: Key Lopc -> String
toEditLopcLink' lid =
  "/" ++ show (linkTo (Proxy :: Proxy ToEditLopc) lid)

toEditLopcLink :: Key Lopc -> AttributeValue
toEditLopcLink lid = stringValue (toEditLopcLink' lid)
