{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TimeSeriesData.Retrieve where

import Servant
import Servant.Client
import Control.Monad.Trans.Either
import Data.List (nub)
import Data.ByteString (ByteString)
import TimeSeriesData.Types
import Time

getTSData :: String -> Int -> [String] -> UTCTime -> UTCTime -> IO TSData
getTSData url port tagNames start end = let tagNames' = nub tagNames in
  mapM (getTSPoints url port start end) tagNames'
  >>= return . TSData (diffUTCTime refTime start) (diffUTCTime refTime end) .
               filter (not . null . snd) . zip tagNames'

getTSPoints :: String -> Int -> UTCTime -> UTCTime -> String -> IO [TSPoint]
getTSPoints url port start end tagName =
  runEitherT (getPoints' url port (Just tagName) (Just start) (Just end))
  >>= return . either (\ _ -> []) (maybe [] id . decode)

type DataAPI = QueryParam "tagname" String
            :> QueryParam "start" UTCTime
            :> QueryParam "end" UTCTime
            :> Get '[OctetStream] ByteString

getPoints' :: String -> Int -> Maybe String -> Maybe UTCTime -> Maybe UTCTime
           -> EitherT ServantError IO ByteString
getPoints' url port = client (Proxy :: Proxy DataAPI) $ BaseUrl Http url port