{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module AppM where

import Servant
import Database.Persist.Postgresql
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Config

type AppM = ReaderT Config (EitherT ServantErr IO)

readerToEither :: Config -> AppM :~> EitherT ServantErr IO
readerToEither cfg = Nat $ \ x -> runReaderT x cfg

runDb :: forall (m :: * -> *) b. (MonadIO m, MonadReader Config m)
      => SqlPersistT IO b -> m b
runDb query = asks getPool >>= liftIO . runSqlPool query
