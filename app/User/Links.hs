{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module User.Links where

import Servant
import Common.Links
import User.Routes

toLoginLink' :: Bool -> String
toLoginLink' isValid = "/" ++ show (linkTo (Proxy :: Proxy ToLogin) isValid)

logoutLink' :: String
logoutLink' = "/" ++ show (linkTo (Proxy :: Proxy Logout))
