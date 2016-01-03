module Blc.Calculate 
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

markCalculate :: UTCTime -> UTCTime -> Key Blc -> AppM ()
markCalculate start end bid = do
  qsem    <- asks getQSem
  counter <- asks getCounter
  src     <- asks getSrcUrl
  port    <- asks getSrcPort
  connStr <- asks getPoolConnStr
  let calcOpts = CalcOpts src port start end
  mBlc <- runDb (get bid)
  case mBlc of
    Nothing -> return ()
    Just blc -> do
      let eb = Entity bid blc
      _ <- liftIO $ forkIO $ calculateThread qsem counter calcOpts connStr eb
      return ()

calculateThread :: QSem -> MVar Int -> CalcOpts -> ByteString -> Entity Blc
                -> IO ()
calculateThread qsem counter calcOpts connStr (Entity kBlc blc) = do
  -- Acquire lock
  waitQSem qsem
  modifyMVar_ counter (return . (+) 1)
  -- Perform the calculations
  (demand, uptimeDemand, uptime, performUptime, mvInterv, spInterv,
   mvSat, cvAffBySat) <- (flip runReaderT) calcOpts $ do
    let filterCounts n = map fst . filter ((== n) . snd)
    demand   <- calcInterval (blcDemandCond blc)
    uptime   <- calcInterval (blcUptimeCond blc)
    let uptimeDemand = filterCounts 2 $ counts [demand, uptime]
    perform  <- calcInterval $
      let margin  = show $ blcMargin blc
          measTag = blcMeasTag blc
          sptTag  = blcSptTag blc
      in case blcObjective blc of
           Near  -> concat ["[", measTag, "]<[", sptTag, "]+", margin ," and "
                           ,"[", measTag, "]>[", sptTag, "]-", margin]
           Above -> concat ["[", sptTag,  "]-[", measTag, "]<", margin]
           Below -> concat ["[", measTag, "]-[", sptTag,  "]<", margin]
    let performUptime = filterCounts 2 $ counts [perform, uptime]
    calcMvI  <- calcInterval (blcCalcMvICond blc)
    calcSpI  <- calcInterval (blcCalcSpICond blc)
    mvInterv <- calcChange (blcOutTag blc) calcMvI
    spInterv <- calcChange (blcSptTag blc) calcSpI
    mvSat    <- calcInterval $
                let outTag = blcOutTag blc
                    outMax = show (blcOutMax blc)
                    outMin = show (blcOutMin blc)
                in concat [ "[", outTag, "]>=", outMax, " or "
                          , "[", outTag, "]<=", outMin ]
    let notPerform = filterCounts 2 $ counts [perform]
    let cvAffBySat = filterCounts 2 $ counts [notPerform, mvSat]
    return (demand, uptimeDemand, uptime, performUptime,
            mvInterv, spInterv, mvSat, cvAffBySat)
  -- Write to database
  let CalcOpts _ _ start end = calcOpts
  let cleanRange' = cleanRange start end kBlc
  let writeIntervals' = writeIntervals start end kBlc
  runNoLoggingT $ withPostgresqlConn connStr $ lift . runReaderT (do
    cleanRange' Demand
    cleanRange' UptimeDemand
    cleanRange' Uptime
    cleanRange' PerformUptime
    deleteWhere [ BlcEventTime>.start, BlcEventTime<=.end, BlcEventBlc==.kBlc ]
    cleanRange' MvSat
    cleanRange' CvAffBySat
    writeIntervals' Demand demand
    writeIntervals' UptimeDemand uptimeDemand
    writeIntervals' Uptime uptime
    writeIntervals' PerformUptime performUptime
    forM_ mvInterv $
      (\t -> insert_ $ BlcEvent kBlc t MvInterv) . ((flip addUTCTime) refTime)
    forM_ spInterv $
      (\t -> insert_ $ BlcEvent kBlc t SpInterv) . ((flip addUTCTime) refTime)
    writeIntervals' MvSat mvSat
    writeIntervals' CvAffBySat cvAffBySat
    )
  -- Release lock
  modifyMVar_ counter (return . (flip (-)) 1)
  signalQSem qsem

calcInterval :: String -> ReaderT CalcOpts IO [TSInterval]
calcInterval condition = case parseExpression condition of
  Left  _          -> return []
  Right condition' -> do
    CalcOpts src port start end <- ask
    tsData <- lift $ getTSData src port (listTags condition') start end
    return (evaluate condition' tsData)

calcChange :: String -> [TSInterval] -> ReaderT CalcOpts IO [NominalDiffTime]
calcChange tagName intervals = do
    CalcOpts src port start end <- ask
    tsPoints <- lift $ getTSPoints src port start end tagName
    return (changesIn tsPoints intervals)

cleanRange :: UTCTime -> UTCTime -> Key Blc -> MetricType -> SqlPersistT IO ()
cleanRange start end kBlc metricType = do
  -- Between start and end
  deleteWhere [ BlcIntervalBlc      ==. kBlc
              , BlcIntervalCategory ==. metricType
              , BlcIntervalStart    >=. start
              , BlcIntervalEnd      <=. end ]
  -- Overlap start
  updateWhere [ BlcIntervalBlc      ==. kBlc
              , BlcIntervalCategory ==. metricType
              , BlcIntervalEnd      >.  start
              , BlcIntervalEnd      <=. end ]
              [ BlcIntervalEnd      =.  start ]
  -- Overlap end
  updateWhere [ BlcIntervalBlc      ==. kBlc
              , BlcIntervalCategory ==. metricType
              , BlcIntervalStart    >=. start
              , BlcIntervalStart    <.  end ]
              [ BlcIntervalStart    =.  end ]
  -- Overlap both start and end
  exceedRanges <- selectList [ BlcIntervalBlc      ==. kBlc
                             , BlcIntervalCategory ==. metricType
                             , BlcIntervalStart    <.  start
                             , BlcIntervalEnd      >.  end ] []
  forM_ exceedRanges (\ (Entity kr r) -> do
    update kr [ BlcIntervalEnd =. start ]
    insert_ $ BlcInterval (blcIntervalBlc r) end (blcIntervalEnd r) metricType)

writeIntervals :: UTCTime -> UTCTime -> Key Blc -> MetricType -> [TSInterval]
               -> SqlPersistT IO ()
writeIntervals start end kBlc metricType intervals =
  forM_ (fmap g intervals) f where
    g (s, e) = ( max (addUTCTime s refTime) start
               , min (addUTCTime e refTime) end   )
    insert_' s e = insert_ $ BlcInterval kBlc s e metricType
    f (s, e)
      | s == start && e < end = do
        numAffected <- E.updateCount $ \ u -> do
          E.set u [ BlcIntervalEnd E.=. E.val e ]
          E.where_ (       (u E.^. BlcIntervalBlc)      E.==. E.val kBlc
                     E.&&. (u E.^. BlcIntervalCategory) E.==. E.val metricType
                     E.&&. (u E.^. BlcIntervalEnd)      E.==. E.val start )
        when (numAffected == 0) (insert_' s e)
      | e == end && s > start = do
        numAffected <- E.updateCount $ \ u -> do
          E.set u [ BlcIntervalStart E.=. E.val s ]
          E.where_ (       (u E.^. BlcIntervalBlc)      E.==. E.val kBlc
                     E.&&. (u E.^. BlcIntervalCategory) E.==. E.val metricType
                     E.&&. (u E.^. BlcIntervalStart)    E.==. E.val end )
        when (numAffected == 0) (insert_' s e)
      | s == start && e == end = do
          atEnd <- selectFirst [ BlcIntervalBlc      ==. kBlc
                               , BlcIntervalCategory ==. metricType
                               , BlcIntervalStart    ==. end ] []
          let latestE = maybe e (blcIntervalEnd . entityVal) atEnd
          when (isJust atEnd) (delete $ entityKey $ fromJust atEnd)
          numAffected <- E.updateCount $ \ u -> do
            E.set u [ BlcIntervalEnd E.=. E.val latestE ]
            E.where_ (       (u E.^. BlcIntervalBlc)      E.==. E.val kBlc
                       E.&&. (u E.^. BlcIntervalCategory) E.==. E.val metricType
                       E.&&. (u E.^. BlcIntervalEnd)      E.==. E.val start )
          when (numAffected == 0) (insert_' s latestE)
      | otherwise = insert_' s e
