{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Home.Links where

import Servant
import Text.Blaze.Html5
import Common.Links
import Home.Routes

homePageLink' :: String
homePageLink' = "/" ++ show (linkTo (Proxy :: Proxy Home))

homePageLink :: AttributeValue
homePageLink = stringValue homePageLink'
