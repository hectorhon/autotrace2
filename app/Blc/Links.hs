{-# LANGUAGE RankNTypes #-}

module Blc.Links where

import Servant
import Database.Persist.Postgresql
import Text.Blaze.Html5
import Data.Time
import Common.Links
import Blc.API
import Area.Types
import Blc.Types

toCreateBlcLink :: Key Area -> AttributeValue
toCreateBlcLink aid = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ToCreateBlc) aid)



viewBlcLink' :: Key Area -> Key Blc -> String
viewBlcLink' aid bid =
  "/" ++ show (linkTo (Proxy :: Proxy ViewBlc) aid bid)

viewBlcLink :: Key Area -> Key Blc -> AttributeValue
viewBlcLink aid bid = stringValue $ viewBlcLink' aid bid



toEditBlcLink :: Key Area -> Key Blc -> AttributeValue
toEditBlcLink aid bid = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ToEditBlc) aid bid)



toCalculateBlcDefaultDayLink :: Key Area -> Key Blc -> AttributeValue
toCalculateBlcDefaultDayLink aid bid = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ToCalculateBlc') aid bid)



viewBlcsPerformanceLink :: Key Area -> Day -> Day -> AttributeValue
viewBlcsPerformanceLink aid start end = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ViewBlcsPerformance) aid start end)

viewBlcsPerformanceDefaultDayLink' :: Key Area -> String
viewBlcsPerformanceDefaultDayLink' aid =
  "/" ++ show (linkTo (Proxy :: Proxy ViewBlcsPerformance') aid)

viewBlcsPerformanceDefaultDayLink :: Key Area -> AttributeValue
viewBlcsPerformanceDefaultDayLink aid = stringValue $
  viewBlcsPerformanceDefaultDayLink' aid


viewBlcBadActorsLink :: Key Area -> Day -> Day -> Double -> Double
                     -> AttributeValue
viewBlcBadActorsLink aid start end complianceTargetPct qualityTargetPct =
  stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ViewBlcBadActors)
                      aid start end complianceTargetPct qualityTargetPct)



toCalculateAreaBlcsLink :: Key Area -> Day -> Day -> AttributeValue
toCalculateAreaBlcsLink aid start end = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ToCalculateAreaBlcs) aid start end)



viewBlcLabelLink' :: Key BlcLabel -> String
viewBlcLabelLink' lid = "/" ++ show (linkTo (Proxy :: Proxy ViewBlcLabel) lid)

viewBlcLabelLink :: Key BlcLabel -> AttributeValue
viewBlcLabelLink = stringValue . viewBlcLabelLink'



viewBlcLabelsLink' :: String
viewBlcLabelsLink' = "/" ++ show (linkTo (Proxy :: Proxy ViewBlcLabels))

viewBlcLabelsLink :: AttributeValue
viewBlcLabelsLink = stringValue viewBlcLabelsLink'



toEditBlcLabelLink :: Key BlcLabel -> AttributeValue
toEditBlcLabelLink lid = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ToEditBlcLabel) lid)
