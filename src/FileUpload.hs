{-|
 - https://www.github.com/haskell-servant/servant/issues/133
 -}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileUpload
  ( module FileUpload
  , FileInfo (..)
  ) where

import Servant
import Control.Monad.Trans.Resource
import Data.ByteString.Lazy (ByteString)
import Network.Wai.Parse

data Mem
data Tmp

class KnownBackEnd b where
  type Storage b :: *
  withBackEnd :: Proxy b -> (BackEnd (Storage b) -> IO r) -> IO r

instance KnownBackEnd Mem where
  type Storage Mem = ByteString
  withBackEnd Proxy f = f lbsBackEnd

instance KnownBackEnd Tmp where
  type Storage Tmp = FilePath
  withBackEnd Proxy f = runResourceT . withInternalState $ \ s ->
    f (tempFileBackEnd s)

data Files b
type FilesMem = Files Mem
type FilesTmp = Files Tmp
type MultiPartData b = ([Param], [File (Storage b)])

instance (KnownBackEnd b, HasServer api) => HasServer (Files b :> api) where
  type ServerT (Files b :> api) m = MultiPartData b -> ServerT api m
  route Proxy subserver req res = withBackEnd pb $ \ b -> do
    dat <- parseRequestBody b req
    route (Proxy :: Proxy api) (subserver dat) req res
    where pb = Proxy :: Proxy b
