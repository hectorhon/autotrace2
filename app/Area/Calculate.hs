{-# LANGUAGE OverloadedStrings #-}

module Area.Calculate 
  ( markCalculateASD
  ) where

import TimeSeriesData
import Database.Persist.Postgresql
import qualified Database.Esqueleto as E
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Concurrent
import Data.Maybe
import Data.ByteString.Char8 (ByteString)
import AppM
import Config
import Area.Types
import Time
import Area.Queries.IntervalsDuring

data CalcOpts = CalcOpts String Int UTCTime UTCTime

-- | Marking on just the target is sufficient,
--   this will get the entire tree (ancestors, self, and descendants)
markCalculateASD :: UTCTime -> UTCTime -> Key Area -> AppM ()
markCalculateASD start end aid = do
  qsemn    <- asks getQSemN
  counter  <- asks getCounter
  src      <- asks getSrcUrl
  port     <- asks getSrcPort
  connStr  <- asks getPoolConnStr
  maxQSemN <- asks getMaxQSemN
  let calcOpts = CalcOpts src port start end
  orderedTargetAreas <- getASD aid
  _ <- liftIO $ forkIO $ do
    waitQSemN qsemn maxQSemN
    modifyMVar_ counter (return . (+) 1)
    forM_ orderedTargetAreas $ \ area -> calculate calcOpts connStr area
    modifyMVar_ counter (return . (flip (-)) 1)
    signalQSemN qsemn maxQSemN
  return ()

-- | Gets ancestors, self, and descendants, in that order
getASD :: Key Area -> AppM [Entity Area]
getASD area = runDb $ rawSql
  "  with recursive t1 as (                                   \
  \    select (0) as level,* from area where id = ?           \
  \    union select level+1,area.* from area                  \
  \          inner join t1 on area.parent = t1.id),           \
  \  t2 as (                                                  \
  \    select (0) as level,* from area where id = ?           \
  \    union select level-1,area.* from area                  \
  \          inner join t2 on area.id = t2.parent)            \
  \  select * from t1 union select * from t2 order by level;  "
  [PersistInt64 $ fromSqlKey area, PersistInt64 $ fromSqlKey area]
  >>= return .
      map (\ (Single l, Single i, Single n, Single d, Single p, Single dc) ->
             let ll = l :: Int in Entity i (Area n d p dc))

calculate :: CalcOpts -> ByteString -> Entity Area -> IO ()
calculate calcOpts connStr (Entity aid area) = do
  -- Perform the (partial) calculation
  demand' <- runReaderT (calcInterval $ areaDemandCond area) calcOpts
  -- Get parent result, AND, then write to database
  let filterCounts n = map fst . filter ((== n) . snd)
  let CalcOpts _ _ start end = calcOpts
  runNoLoggingT $ withPostgresqlConn connStr $ lift . runReaderT (do
    demand <- case areaParent area of
      Nothing -> return demand'
      Just parent -> do
        parentDemand <- intervalsDuring start end parent
        return $ filterCounts 2 $ counts [parentDemand, demand']
    cleanRange start end aid
    writeIntervals start end aid demand
    )

calcInterval :: String -> ReaderT CalcOpts IO [TSInterval]
calcInterval condition = case parseExpression condition of
  Left  _          -> return []
  Right condition' -> do
    CalcOpts src port start end <- ask
    tsData <- lift $ getTSData src port (listTags condition') start end
    return (evaluate condition' tsData)

cleanRange :: UTCTime -> UTCTime -> Key Area -> SqlPersistT IO ()
cleanRange start end aid = do
  -- Between start and end
  deleteWhere [ AreaIntervalArea     ==. aid
              , AreaIntervalStart    >=. start
              , AreaIntervalEnd      <=. end ]
  -- Overlap start
  updateWhere [ AreaIntervalArea     ==. aid
              , AreaIntervalEnd      >.  start
              , AreaIntervalEnd      <=. end ]
              [ AreaIntervalEnd      =.  start ]
  -- Overlap end
  updateWhere [ AreaIntervalArea     ==. aid
              , AreaIntervalStart    >=. start
              , AreaIntervalStart    <.  end ]
              [ AreaIntervalStart    =.  end ]
  -- Overlap both start and end
  exceedRanges <- selectList [ AreaIntervalArea      ==. aid
                             , AreaIntervalStart    <.  start
                             , AreaIntervalEnd      >.  end ] []
  forM_ exceedRanges (\ (Entity kr r) -> do
    update kr [ AreaIntervalEnd =. start ]
    insert_ $
      AreaInterval (areaIntervalArea r) end (areaIntervalEnd r) )

writeIntervals :: UTCTime -> UTCTime -> Key Area -> [TSInterval]
               -> SqlPersistT IO ()
writeIntervals start end aid intervals =
  forM_ (fmap g intervals) f where
    g (s, e) = ( max (addUTCTime s refTime) start
               , min (addUTCTime e refTime) end   )
    insert_' s e = insert_ $ AreaInterval aid s e
    f (s, e)
      | s == start && e < end = do
        numAffected <- E.updateCount $ \ u -> do
          E.set u [ AreaIntervalEnd E.=. E.val e ]
          E.where_ (       (u E.^. AreaIntervalArea)     E.==. E.val aid
                     E.&&. (u E.^. AreaIntervalEnd)      E.==. E.val start )
        when (numAffected == 0) (insert_' s e)
      | e == end && s > start = do
        numAffected <- E.updateCount $ \ u -> do
          E.set u [ AreaIntervalStart E.=. E.val s ]
          E.where_ (       (u E.^. AreaIntervalArea)     E.==. E.val aid
                     E.&&. (u E.^. AreaIntervalStart)    E.==. E.val end )
        when (numAffected == 0) (insert_' s e)
      | s == start && e == end = do
          atEnd <- selectFirst [ AreaIntervalArea     ==. aid
                               , AreaIntervalStart    ==. end ] []
          let latestE = maybe e (areaIntervalEnd . entityVal) atEnd
          when (isJust atEnd) (delete $ entityKey $ fromJust atEnd)
          numAffected <- E.updateCount $ \ u -> do
            E.set u [ AreaIntervalEnd E.=. E.val latestE ]
            E.where_ (      (u E.^. AreaIntervalArea)     E.==. E.val aid
                      E.&&. (u E.^. AreaIntervalEnd)      E.==. E.val start)
          when (numAffected == 0) (insert_' s latestE)
      | otherwise = insert_' s e
