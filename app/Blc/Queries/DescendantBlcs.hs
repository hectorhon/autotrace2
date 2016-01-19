{-# LANGUAGE OverloadedStrings #-}

module Blc.Queries.DescendantBlcs
  ( descendantBlcsOf
  ) where

import Database.Persist.Postgresql
import Area.Types
import Blc.Types
import AppM

descendantBlcsOf :: Key Area -> AppM [Key Blc]
descendantBlcsOf area = runDb $ rawSql
  " with recursive t as (                       \
  \   select * from area where id = ?           \
  \   union select area.* from area             \
  \         inner join t on area.parent = t.id) \
  \ select blc.id from blc inner join t on      \
  \ blc.area = t.id                             "
  [PersistInt64 $ fromSqlKey area]
  >>= return . map (toSqlKey . unSingle)
