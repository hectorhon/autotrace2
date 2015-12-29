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



toCreateApcCvLink :: Key Area -> Key Apc -> AttributeValue
toCreateApcCvLink aid apcId = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ToCreateApcCv) aid apcId)



viewApcCvLink' :: Key Area -> Key Apc -> Key Cv -> String
viewApcCvLink' aid apcId cid =
  "/" ++ show (linkTo (Proxy :: Proxy ViewApcCv) aid apcId cid)

viewApcCvLink :: Key Area -> Key Apc -> Key Cv -> AttributeValue
viewApcCvLink aid apcId cid = stringValue $ viewApcCvLink' aid apcId cid
