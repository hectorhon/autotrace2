module Blc.Calculate 
  ( markCalculate
  ) where

import TimeSeriesData
import Database.Persist.Postgresql
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Concurrent
import Data.ByteString.Char8 (ByteString)
import AppM
import Config
import Schema
import Time

data CalcOpts = CalcOpts String Int UTCTime UTCTime

markCalculate :: UTCTime -> UTCTime -> Entity Blc -> AppM ()
markCalculate start end blc = do
  qsem    <- asks getQSem
  counter <- asks getCounter
  src     <- asks getSrcUrl
  port    <- asks getSrcPort
  connStr <- asks getPoolConnStr
  let calcOpts = CalcOpts src port start end
  _       <- liftIO $ forkIO $ calculateThread qsem counter calcOpts connStr blc
  return ()

calculateThread :: QSem -> MVar Int -> CalcOpts -> ByteString -> Entity Blc
                -> IO ()
calculateThread qsem counter calcOpts connStr (Entity kBlc blc) = do
  -- Acquire lock
  waitQSem qsem
  modifyMVar_ counter (return . (+) 1)
  -- Perform the calculations
  (demand, uptimeDemand, uptime, performUptime, modeInterv, mvInterv, spInterv,
   mvSat, cvAffBySat) <- (flip runReaderT) calcOpts $ do
    let filterCounts n = map fst . filter ((== n) . snd)
    demand   <- calcInterval (blcDemandCond blc)
    uptime   <- calcInterval (blcUptimeCond blc)
    let uptimeDemand = filterCounts 2 $ counts [demand, uptime]
    let modeInterv = map fst (filterCounts 0 $ counts [uptime])
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
    return (demand, uptimeDemand, uptime, performUptime, modeInterv,
            mvInterv, spInterv, mvSat, cvAffBySat)
  -- Write to database
  let CalcOpts _ _ start end = calcOpts
  runNoLoggingT $ withPostgresqlConn connStr $ lift . runReaderT (do
    deleteWhere [ BlcResultStart >=. start
                , BlcResultStart <=. end
                , BlcResultBlc   ==. kBlc ]
    insert_ (BlcResult kBlc start end (durationOf demand)
                                      (durationOf uptimeDemand)
                                      (durationOf uptime)
                                      (durationOf performUptime)
                                      (durationOf mvSat)
                                      (durationOf cvAffBySat))
    deleteWhere [ BlcEventTime >.  start
                , BlcEventTime <=. end
                , BlcEventBlc  ==. kBlc ]
    forM_ mvInterv $
      (\t -> insert_ $ BlcEvent kBlc t MvInterv) . ((flip addUTCTime) refTime)
    forM_ spInterv $
      (\t -> insert_ $ BlcEvent kBlc t SpInterv) . ((flip addUTCTime) refTime)
    forM_ modeInterv $
      (\t -> insert_ $ BlcEvent kBlc t ModeInterv) . ((flip addUTCTime) refTime)
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

durationOf :: [TSInterval] -> Double
durationOf = realToFrac . foldr ((+) . f) 0
  where f (start, end) = end - start
