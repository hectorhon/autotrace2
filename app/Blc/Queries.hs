module Blc.Queries 
  ( AreaResult (AreaResult)
  , BlcResult (BlcResult)
  , getResult
  , getBadCompliances
  , getBadQualities
  , descendantBlcsOf
  ) where

import Database.Esqueleto
import Data.Time
import Data.Maybe (catMaybes)
import Blc.Queries.AreaResult
import Blc.Queries.BlcResult
import Blc.Queries.DescendantBlcs
import Area.Types
import AppM

getResult :: UTCTime -> UTCTime -> Key Area
          -> AppM (Maybe (AreaResult, [AreaResult], [BlcResult]))
getResult start end aid = do
  mAreaResult <- getAreaResult start end aid
  case mAreaResult of
    Nothing -> return Nothing
    Just areaResult -> do
      subareas <- runDb $ select $ from $ \ a -> do
        where_ (a ^. AreaParent ==. val (Just aid))
        orderBy [asc (a ^. AreaName)]
        return (a ^. AreaId)
      subareaResults <- mapM (getAreaResult start end) (map unValue subareas)
      blcResults <- getChildBlcResult start end aid
      return $ Just (areaResult, catMaybes subareaResults, blcResults)
