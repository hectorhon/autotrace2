{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module User.GetUser where

import Servant
import Database.Persist.Postgresql
import Network.Wai
import User.Types
import Data.ByteString.Char8 (unpack)

data GetUser

instance forall a. HasServer a => HasServer (GetUser :> a) where
  type ServerT (GetUser :> a) m = Maybe (Entity User) -> ServerT a m
  route (Proxy :: Proxy (GetUser :> a)) a rq k =
    route (Proxy :: Proxy a) (a mUser) rq k
    where mUser = do userStr <- lookup "User" (requestHeaders rq)
                     user <- case (reads (unpack userStr)) of
                               [] -> Nothing
                               u:_ -> Just (fst u)
                     return user
type instance IsElem' e (GetUser :> a) = IsElem e a
