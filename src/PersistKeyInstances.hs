{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistKeyInstances where

import Database.Persist.Postgresql
import Servant
import Data.Text (pack)
import Data.Text.Read (Reader, decimal)

instance ToBackendKey SqlBackend a => FromText (Key a) where
  fromText = either (\ _ -> Nothing) (Just . toSqlKey . fromIntegral . fst)
             . (decimal :: Reader Integer)

instance ToBackendKey SqlBackend a => ToText (Key a) where
  toText = pack . show . fromSqlKey
