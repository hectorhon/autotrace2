{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module User.Links where

import Servant
import Common.Links
import User.Routes

toLoginLink' :: String
toLoginLink' = "/" ++ show (linkTo (Proxy :: Proxy ToLogin))
