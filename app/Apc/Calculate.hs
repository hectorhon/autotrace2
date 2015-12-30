module Apc.Calculate 
  ( markCalculate
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
import Schema
import Time

data CalcOpts = CalcOpts String Int UTCTime UTCTime

markCalculate :: UTCTime -> UTCTime -> Entity Apc -> AppM ()
markCalculate start end apc = do
  qsem    <- asks getQSem
  counter <- asks getCounter
  src     <- asks getSrcUrl
  port    <- asks getSrcPort
  connStr <- asks getPoolConnStr
  let calcOpts = CalcOpts src port start end
  _       <- liftIO $ forkIO $ calculateThread qsem counter calcOpts connStr apc
  return ()

calculateThread :: QSem -> MVar Int -> CalcOpts -> ByteString -> Entity Apc
                -> IO ()
calculateThread qsem counter calcOpts connStr (Entity aid apc) = do
  waitQSem qsem
  modifyMVar_ counter (return . (+) 1)
  uptime <- runReaderT (calcInterval $ apcUptimeCond apc) calcOpts
  let CalcOpts _ _ start end = calcOpts
  runNoLoggingT $ withPostgresqlConn connStr $ lift . runReaderT (do
    cvs <- selectList [CvApc ==. aid] []
    let cids = map entityKey cvs
    cvExceeds <- liftIO $ (flip runReaderT) calcOpts $
      mapM (calcInterval . mkCvExceedCondition . entityVal) cvs
    -- Write to database
    cleanRangeApc start end Uptime aid
    writeIntervalsApc start end Uptime aid uptime
    mapM_ (cleanRangeCv start end Exceed) cids
    mapM_ (uncurry (writeIntervalsCv start end Exceed)) (zip cids cvExceeds)
    )
  modifyMVar_ counter (return . (flip (-)) 1)
  signalQSem qsem

calcInterval :: String -> ReaderT CalcOpts IO [TSInterval]
calcInterval condition = case parseExpression condition of
  Left  _          -> return []
  Right condition' -> do
    CalcOpts src port start end <- ask
    tsData <- lift $ getTSData src port (listTags condition') start end
    return (evaluate condition' tsData)

mkCvExceedCondition :: Cv -> String
mkCvExceedCondition cv = measTag ++ ">" ++ srhTag ++ " or "
                      ++ measTag ++ "<" ++ srlTag
  where measTag = numOrTag (cvMeasTag cv)
        srhTag  = numOrTag (cvSrhTag cv)
        srlTag  = numOrTag (cvSrlTag cv)
        numOrTag x = maybe ("[" ++ x ++ "]")
                           (\ (_, s) -> if null s then x else ("[" ++ x ++ "]"))
                           (listToMaybe (reads x :: [(Double, String)]))

cleanRangeApc :: UTCTime -> UTCTime -> MetricType -> Key Apc
              -> SqlPersistT IO ()
cleanRangeApc start end metricType aid = do
  -- Between start and end
  deleteWhere [ ApcIntervalApc      ==. aid
              , ApcIntervalCategory ==. metricType
              , ApcIntervalStart    >=. start
              , ApcIntervalEnd      <=. end ]
  -- Overlap start
  updateWhere [ ApcIntervalApc      ==. aid
              , ApcIntervalCategory ==. metricType
              , ApcIntervalEnd      >.  start
              , ApcIntervalEnd      <=. end ]
              [ ApcIntervalEnd      =.  start ]
  -- Overlap end
  updateWhere [ ApcIntervalApc      ==. aid
              , ApcIntervalCategory ==. metricType
              , ApcIntervalStart    >=. start
              , ApcIntervalStart    <.  end ]
              [ ApcIntervalStart    =.  end ]
  -- Overlap both start and end
  exceedRanges <- selectList [ ApcIntervalApc      ==. aid
                             , ApcIntervalCategory ==. metricType
                             , ApcIntervalStart    <.  start
                             , ApcIntervalEnd      >.  end ] []
  forM_ exceedRanges (\ (Entity kr r) -> do
    update kr [ ApcIntervalEnd =. start ]
    insert_ $ ApcInterval (apcIntervalApc r) end (apcIntervalEnd r) metricType)

cleanRangeCv :: UTCTime -> UTCTime -> MetricType -> Key Cv -> SqlPersistT IO ()
cleanRangeCv start end metricType cid = do
  -- Between start and end
  deleteWhere [ CvIntervalCv       ==. cid
              , CvIntervalCategory ==. metricType
              , CvIntervalStart    >=. start
              , CvIntervalEnd      <=. end ]
  -- Overlap start
  updateWhere [ CvIntervalCv       ==. cid
              , CvIntervalCategory ==. metricType
              , CvIntervalEnd      >.  start
              , CvIntervalEnd      <=. end ]
              [ CvIntervalEnd      =.  start ]
  -- Overlap end
  updateWhere [ CvIntervalCv       ==. cid
              , CvIntervalCategory ==. metricType
              , CvIntervalStart    >=. start
              , CvIntervalStart    <.  end ]
              [ CvIntervalStart    =.  end ]
  -- Overlap both start and end
  exceedRanges <- selectList [ CvIntervalCv       ==. cid
                             , CvIntervalCategory ==. metricType
                             , CvIntervalStart    <.  start
                             , CvIntervalEnd      >.  end ] []
  forM_ exceedRanges (\ (Entity kr r) -> do
    update kr [ CvIntervalEnd =. start ]
    insert_ $ CvInterval (cvIntervalCv r) end (cvIntervalEnd r) metricType)

writeIntervalsApc :: UTCTime -> UTCTime -> MetricType -> Key Apc -> [TSInterval]
                  -> SqlPersistT IO ()
writeIntervalsApc start end metricType aid intervals =
  forM_ (fmap g intervals) f where
    g (s, e) = ( max (addUTCTime s refTime) start
               , min (addUTCTime e refTime) end   )
    insert_' s e = insert_ $ ApcInterval aid s e metricType
    f (s, e)
      | s == start && e < end = do
        numAffected <- E.updateCount $ \ u -> do
          E.set u [ ApcIntervalEnd E.=. E.val e ]
          E.where_ (       (u E.^. ApcIntervalApc)      E.==. E.val aid
                     E.&&. (u E.^. ApcIntervalCategory) E.==. E.val metricType
                     E.&&. (u E.^. ApcIntervalEnd)      E.==. E.val start )
        when (numAffected == 0) (insert_' s e)
      | e == end && s > start = do
        numAffected <- E.updateCount $ \ u -> do
          E.set u [ ApcIntervalStart E.=. E.val s ]
          E.where_ (       (u E.^. ApcIntervalApc)      E.==. E.val aid
                     E.&&. (u E.^. ApcIntervalCategory) E.==. E.val metricType
                     E.&&. (u E.^. ApcIntervalStart)    E.==. E.val end )
        when (numAffected == 0) (insert_' s e)
      | s == start && e == end = do
          atEnd <- selectFirst [ ApcIntervalApc      ==. aid
                               , ApcIntervalCategory ==. metricType
                               , ApcIntervalStart    ==. end ] []
          let latestE = maybe e (apcIntervalEnd . entityVal) atEnd
          when (isJust atEnd) (delete $ entityKey $ fromJust atEnd)
          numAffected <- E.updateCount $ \ u -> do
            E.set u [ ApcIntervalEnd E.=. E.val latestE ]
            E.where_ (       (u E.^. ApcIntervalApc)      E.==. E.val aid
                       E.&&. (u E.^. ApcIntervalCategory) E.==. E.val metricType
                       E.&&. (u E.^. ApcIntervalEnd)      E.==. E.val start )
          when (numAffected == 0) (insert_' s latestE)
      | otherwise = insert_' s e

writeIntervalsCv :: UTCTime -> UTCTime -> MetricType -> Key Cv -> [TSInterval]
                  -> SqlPersistT IO ()
writeIntervalsCv start end metricType cid intervals =
  forM_ (fmap g intervals) f where
    g (s, e) = ( max (addUTCTime s refTime) start
               , min (addUTCTime e refTime) end   )
    insert_' s e = insert_ $ CvInterval cid s e metricType
    f (s, e)
      | s == start && e < end = do
        numAffected <- E.updateCount $ \ u -> do
          E.set u [ CvIntervalEnd E.=. E.val e ]
          E.where_ (       (u E.^. CvIntervalCv)      E.==. E.val cid
                     E.&&. (u E.^. CvIntervalCategory) E.==. E.val metricType
                     E.&&. (u E.^. CvIntervalEnd)      E.==. E.val start )
        when (numAffected == 0) (insert_' s e)
      | e == end && s > start = do
        numAffected <- E.updateCount $ \ u -> do
          E.set u [ CvIntervalStart E.=. E.val s ]
          E.where_ (       (u E.^. CvIntervalCv)      E.==. E.val cid
                     E.&&. (u E.^. CvIntervalCategory) E.==. E.val metricType
                     E.&&. (u E.^. CvIntervalStart)    E.==. E.val end )
        when (numAffected == 0) (insert_' s e)
      | s == start && e == end = do
          atEnd <- selectFirst [ CvIntervalCv      ==. cid
                               , CvIntervalCategory ==. metricType
                               , CvIntervalStart    ==. end ] []
          let latestE = maybe e (cvIntervalEnd . entityVal) atEnd
          when (isJust atEnd) (delete $ entityKey $ fromJust atEnd)
          numAffected <- E.updateCount $ \ u -> do
            E.set u [ CvIntervalEnd E.=. E.val latestE ]
            E.where_ (       (u E.^. CvIntervalCv)      E.==. E.val cid
                       E.&&. (u E.^. CvIntervalCategory) E.==. E.val metricType
                       E.&&. (u E.^. CvIntervalEnd)      E.==. E.val start )
          when (numAffected == 0) (insert_' s latestE)
      | otherwise = insert_' s e
