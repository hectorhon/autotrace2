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
import Data.ByteString.Char8 (unpack)
import User.Enums

class RoleClass a
data ReadRole'
data WriteRole'
instance RoleClass ReadRole'
instance RoleClass WriteRole'

data RequireAuth r

instance forall a. HasServer a => HasServer (RequireAuth ReadRole' :> a) where
  type ServerT (RequireAuth ReadRole' :> a) m = ServerT a m
  route (Proxy :: Proxy (RequireAuth ReadRole' :> a)) a rq k =
    let headers = requestHeaders rq
        roleExist = maybe False
                          (not . null . filter (== ReadRole))
                          (fmap (read . unpack) $ lookup "Roles" headers)
    in if roleExist then route (Proxy :: Proxy a) a rq k
       else k $ succeedWith $
            responseLBS status401 [] "You don't have permission to do this"

instance forall a. HasServer a => HasServer (RequireAuth WriteRole' :> a) where
  type ServerT (RequireAuth WriteRole' :> a) m = ServerT a m
  route (Proxy :: Proxy (RequireAuth WriteRole' :> a)) a rq k =
    let headers = requestHeaders rq
        roleExist = maybe False
                          (not . null . filter (== WriteRole))
                          (fmap (read . unpack) $ lookup "Roles" headers)
    in if roleExist then route (Proxy :: Proxy a) a rq k
       else k $ succeedWith $
            responseLBS status401 [] "You don't have permission to do this"

instance forall a r. (HasLink a, RoleClass r)
         => HasLink (RequireAuth r :> a) where
  type MkLink (RequireAuth r :> a) = MkLink a
  toLink _ = toLink (Proxy :: Proxy a)

type instance IsElem' e (RequireAuth ReadRole' :> a) = IsElem e a
type instance IsElem' e (RequireAuth WriteRole' :> a) = IsElem e a
