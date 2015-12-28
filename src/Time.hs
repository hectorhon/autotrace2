{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Time
  ( module Time
  , module Data.Time
  ) where

import Data.Time
import Servant
import Network.HTTP.Types (urlEncode)
import Control.Monad.Except
import Data.Text (pack, unpack)
import qualified Data.ByteString.Char8 as C (pack, unpack)

-- * Time references

tz :: TimeZone
tz = TimeZone (8*60) False "+8"

refTime :: UTCTime
refTime = UTCTime (fromGregorian 1970 1 1) 0

-- * Servant instances

urlEncode' :: String -> String
urlEncode' = C.unpack . urlEncode True . C.pack

instance ToText Day where
  toText = pack . urlEncode' . formatDay . flip UTCTime 0

instance FromText Day where
  fromText = parseDay . unpack

instance ToText UTCTime where
  toText = pack.show.(round::NominalDiffTime->Integer).flip diffUTCTime refTime

instance FromFormUrlEncoded (Day, Day) where
  fromFormUrlEncoded params = runExcept $ do
    start <- maybe (throwError "Missing start")
                   (maybe (throwError "Invalid start") return
                    . parseDay . unpack)
                   (lookup "start" params)
    end <- maybe (throwError "Missing end")
                 (maybe (throwError "Invalid end") return . parseDay . unpack)
                 (lookup "end" params)
    return (start, end)

-- * Parse from a string

parseDay :: String -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%d . %m . %Y"

parseClock :: String -> Maybe TimeOfDay
parseClock = parseTimeM True defaultTimeLocale "%R"

-- * Format to a string

formatDay :: UTCTime -> String
formatDay = formatTime defaultTimeLocale "%d . %m . %Y" . utcToLocalTime tz

formatShort :: UTCTime -> String
formatShort = formatTime defaultTimeLocale "%d/%m %R". utcToLocalTime tz

formatNormal :: UTCTime -> String
formatNormal = formatTime defaultTimeLocale "%d . %m . %Y %R" .utcToLocalTime tz

formatClock :: UTCTime -> String
formatClock = formatTime defaultTimeLocale "%R" . utcToLocalTime tz

-- * Relative time

relativeDay :: Integer -> IO UTCTime
relativeDay offset = do
  UTCTime day _ <- getCurrentTime
  return $ UTCTime (addDays offset day) 0
