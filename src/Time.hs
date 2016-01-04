{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Time
  ( module Time
  , module Data.Time
  ) where

import Data.Time
import Servant
import Database.Persist.Postgresql
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B (ByteString, unpack)
import Control.Exception (assert)
import Control.Monad.Except
import Control.Applicative ((<|>))
import Data.Text (pack, unpack, append)

-- * Time references

tz :: TimeZone
tz = TimeZone (8*60) False "+8"

refTime :: UTCTime
refTime = UTCTime (fromGregorian 1970 1 1) 0

-- * Servant instances

instance ToText Day where
  toText = pack . formatDay . flip UTCTime 0

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

-- * Postgresql interval to NominalDiffTime

-- | Parses positive intervals only
decodePGInterval :: B.ByteString -> Maybe NominalDiffTime
decodePGInterval raw = maybeResult $ feed (parse p raw) "\n"
  where p = choice [p1, p2, p3]
        p1 = do d <- decimal :: Parser Int
                _ <- string " days " <|> string " day "
                h <- decimal :: Parser Int
                _ <- char ':'
                m <- decimal :: Parser Int
                _ <- char ':'
                s <- double
                _ <- endOfLine
                return $ (realToFrac :: Double -> NominalDiffTime) $
                  realToFrac d * 86400
                  + realToFrac h * 3600
                  + realToFrac m * 60
                  + s
        p2 = do d <- decimal :: Parser Int
                _ <- string " days" <|> string " day"
                _ <- endOfLine
                return $ realToFrac (realToFrac d * 86400 :: Double)
        p3 = do h <- decimal :: Parser Int
                _ <- char ':'
                m <- decimal :: Parser Int
                _ <- char ':'
                s <- double
                _ <- endOfLine
                return $ realToFrac $ realToFrac h * 3600
                                    + realToFrac m * 60
                                    + s

instance PersistField NominalDiffTime where
  toPersistValue = assert False undefined
  fromPersistValue (PersistDbSpecific str) = maybe
    (Left (append "Failed to parse " (pack $ B.unpack str)))
    (Right)
    (decodePGInterval str)
  fromPersistValue _ = assert False undefined

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
relativeDay offset = getCurrentTime
  >>= return . localDayToUTC . addDays offset . utcToLocalDay

localDayToUTC :: Day -> UTCTime
localDayToUTC = localTimeToUTC tz . (flip LocalTime) midnight

utcToLocalDay :: UTCTime -> Day
utcToLocalDay = localDay . utcToLocalTime tz
