{-# LANGUAGE RankNTypes #-}

module Apc.Links where

import Servant
import Database.Persist.Postgresql
import Text.Blaze.Html5
import Data.Time (Day)
import Common.Links
import Apc.API
import Schema

toCreateApcLink :: Key Area -> AttributeValue
toCreateApcLink aid = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ToCreateApc) aid)



viewApcLink' :: Key Area -> Key Apc -> String
viewApcLink' aid apcId =
  "/" ++ show (linkTo (Proxy :: Proxy ViewApc) aid apcId)

viewApcLink :: Key Area -> Key Apc -> AttributeValue
viewApcLink aid apcId = stringValue $ viewApcLink' aid apcId



viewApcsLink :: AttributeValue
viewApcsLink = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ViewApcs))



toEditApcLink :: Key Area -> Key Apc -> AttributeValue
toEditApcLink aid apcId = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ToEditApc) aid apcId)



toCreateApcCvLink :: Key Area -> Key Apc -> AttributeValue
toCreateApcCvLink aid apcId = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ToCreateApcCv) aid apcId)



toCalculateApcLink :: Key Area -> Key Apc -> Day -> Day -> AttributeValue
toCalculateApcLink aid apcId start end = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ToCalculateApc) aid apcId start end)

toCalculateApcDefaultDayLink :: Key Area -> Key Apc -> AttributeValue
toCalculateApcDefaultDayLink aid apcId = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ToCalculateApc') aid apcId)



viewApcPerformanceLink :: Key Area -> Key Apc -> Day -> Day
                       -> AttributeValue
viewApcPerformanceLink aid apcId start end = stringValue $
  "/"
  ++ show (linkTo (Proxy :: Proxy ViewApcPerformance) aid apcId start end)

viewApcPerformanceDefaultDayLink :: Key Area -> Key Apc -> AttributeValue
viewApcPerformanceDefaultDayLink aid apcId = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ViewApcPerformance') aid apcId)



viewApcCvLink' :: Key Area -> Key Apc -> Key Cv -> String
viewApcCvLink' aid apcId cid =
  "/" ++ show (linkTo (Proxy :: Proxy ViewApcCv) aid apcId cid)

viewApcCvLink :: Key Area -> Key Apc -> Key Cv -> AttributeValue
viewApcCvLink aid apcId cid = stringValue $ viewApcCvLink' aid apcId cid



toEditApcCvLink :: Key Area -> Key Apc -> Key Cv -> AttributeValue
toEditApcCvLink aid apcId cid = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ToEditApcCv) aid apcId cid)



viewApcCvTrendLink :: Key Area -> Key Apc -> Key Cv -> Day -> Day
                   -> AttributeValue
viewApcCvTrendLink aid apcId cid start end = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ViewApcCvTrend) aid apcId cid start end)

viewApcCvTrendDefaultDayLink :: Key Area -> Key Apc -> Key Cv -> AttributeValue
viewApcCvTrendDefaultDayLink aid apcId cid = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ViewApcCvTrend') aid apcId cid)
