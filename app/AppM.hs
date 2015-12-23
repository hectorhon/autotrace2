{-# LANGUAGE TypeOperators #-}

module AppM where

import Servant
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Config

type AppM = ReaderT Config (EitherT ServantErr IO)

readerToEither :: Config -> AppM :~> EitherT ServantErr IO
readerToEither cfg = Nat $ \ x -> runReaderT x cfg
