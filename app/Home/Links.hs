{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Home.Links where

import Servant
import Common.Links
import Home.Routes

homePageLink' :: String
homePageLink' = "/" ++ show (linkTo (Proxy :: Proxy Home))
