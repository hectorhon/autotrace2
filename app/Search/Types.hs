{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Search.Types where

import Text.Blaze.Html5 (AttributeValue)
import Database.Persist.Postgresql
import Area.Types
import Area.Links
import Blc.Types
import Blc.Links

class Search a where
  name :: a -> String
  description :: a -> String
  link :: a -> AttributeValue
  category :: a -> String

data Searchable = forall a. Search a => Searchable a

pack :: Search a => a -> Searchable
pack = Searchable

instance Search Searchable where
  name (Searchable a) = name a
  description (Searchable a) = description a
  link (Searchable a) = link a
  category (Searchable a) = category a

instance Search (Entity Area) where
  name = areaName . entityVal
  description = areaDescription . entityVal
  link = viewAreaLink . entityKey
  category _ = "Area"

instance Search (Entity Blc) where
  name = blcName . entityVal
  description = blcDescription . entityVal
  link (Entity bid blc) = viewBlcLink (blcArea blc) bid
  category _ = "Base layer controller"
