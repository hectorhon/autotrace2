{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module User.RequireAuth where

import Servant
import Servant.Server.Internal (succeedWith, RouteResult)
import Network.Wai
import Network.HTTP.Types
import Data.ByteString.Char8 (unpack)
import User.Enums

instance forall a r. (HasLink a, RoleClass r)
         => HasLink (RequireAuth r :> a) where
  type MkLink (RequireAuth r :> a) = MkLink a
  toLink _ = toLink (Proxy :: Proxy a)

roleExist :: RoleType -> Request -> Bool
roleExist role rq =
  maybe False (not . null . filter (== role))
              (fmap (read . unpack) (lookup "Roles" (requestHeaders rq)))

denyAccess :: RouteResult Response
denyAccess =
  succeedWith (responseLBS status401 [] "You don't have permission to do this")

data RequireAuth r

class RoleClass a

data LopcUserRole'
instance RoleClass LopcUserRole'
instance forall a. HasServer a
  => HasServer (RequireAuth LopcUserRole' :> a) where
  type ServerT (RequireAuth LopcUserRole' :> a) m = ServerT a m
  route (Proxy :: Proxy (RequireAuth LopcUserRole' :> a)) a rq k =
    if roleExist LopcUserRole rq
    then route (Proxy :: Proxy a) a rq k
    else k denyAccess
type instance IsElem' e (RequireAuth LopcUserRole' :> a) = IsElem e a

data LopcAdminRole'
instance RoleClass LopcAdminRole'
instance forall a. HasServer a
  => HasServer (RequireAuth LopcAdminRole' :> a) where
  type ServerT (RequireAuth LopcAdminRole' :> a) m = ServerT a m
  route (Proxy :: Proxy (RequireAuth LopcAdminRole' :> a)) a rq k =
    if roleExist LopcAdminRole rq
    then route (Proxy :: Proxy a) a rq k
    else k denyAccess
type instance IsElem' e (RequireAuth LopcAdminRole' :> a) = IsElem e a

data ControlAdminRole'
instance RoleClass ControlAdminRole'
instance forall a. HasServer a
  => HasServer (RequireAuth ControlAdminRole' :> a) where
  type ServerT (RequireAuth ControlAdminRole' :> a) m = ServerT a m
  route (Proxy :: Proxy (RequireAuth ControlAdminRole' :> a)) a rq k =
    if roleExist ControlAdminRole rq
    then route (Proxy :: Proxy a) a rq k
    else k denyAccess
type instance IsElem' e (RequireAuth ControlAdminRole' :> a) = IsElem e a

data AreaAdminRole'
instance RoleClass AreaAdminRole'
instance forall a. HasServer a
  => HasServer (RequireAuth AreaAdminRole' :> a) where
  type ServerT (RequireAuth AreaAdminRole' :> a) m = ServerT a m
  route (Proxy :: Proxy (RequireAuth AreaAdminRole' :> a)) a rq k =
    if roleExist AreaAdminRole rq
    then route (Proxy :: Proxy a) a rq k
    else k denyAccess
type instance IsElem' e (RequireAuth AreaAdminRole' :> a) = IsElem e a

data ManageUsersRole'
instance RoleClass ManageUsersRole'
instance forall a. HasServer a
  => HasServer (RequireAuth ManageUsersRole' :> a) where
  type ServerT (RequireAuth ManageUsersRole' :> a) m = ServerT a m
  route (Proxy :: Proxy (RequireAuth ManageUsersRole' :> a)) a rq k =
    if roleExist ManageUsersRole rq
    then route (Proxy :: Proxy a) a rq k
    else k denyAccess
type instance IsElem' e (RequireAuth ManageUsersRole' :> a) = IsElem e a
