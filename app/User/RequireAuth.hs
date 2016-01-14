{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module User.RequireAuth where

import Servant
import Servant.Server.Internal (succeedWith)
import Network.Wai
import Network.HTTP.Types
import Data.Text (Text)
import Data.ByteString.Char8 (unpack)
import User.Types

data RequireAuth

instance forall a. HasServer a => HasServer (RequireAuth :> a) where
  type ServerT (RequireAuth :> a) m = (Text, [RoleType]) -> ServerT a m
  route Proxy a rq k =
    case lookup "UserRoles" (requestHeaders rq) of
      Nothing -> k $ succeedWith $ responseLBS status401 [] ""
      Just ur -> route (Proxy :: Proxy a) (a $ read $ unpack ur) rq k

instance forall a. HasLink a => HasLink (RequireAuth :> a) where
  type MkLink (RequireAuth :> a) = MkLink a
  toLink _ = toLink (Proxy :: Proxy a)

type instance IsElem' e (RequireAuth :> a) = IsElem e a
