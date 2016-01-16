{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Common.Links where

import Servant
import API

linkTo :: (IsElem endpoint Site, HasLink endpoint)
       => Proxy endpoint -> MkLink endpoint
linkTo = safeLink (Proxy :: Proxy Site)

homePageLink' :: String
homePageLink' = "/" ++ show (linkTo (Proxy :: Proxy HomePage))
