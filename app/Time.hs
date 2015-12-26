module Time
  ( module Time
  , module Data.Time
  ) where

import Data.Time

tz :: TimeZone
tz = TimeZone (8*60) False "+8"

parseDay :: String -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%d . %m . %Y"

parseClock :: String -> Maybe TimeOfDay
parseClock = parseTimeM True defaultTimeLocale "%R"

formatDay :: UTCTime -> String
formatDay = formatTime defaultTimeLocale "%d . %m . %Y" . utcToLocalTime tz

formatShort :: UTCTime -> String
formatShort = formatTime defaultTimeLocale "%d/%m %R". utcToLocalTime tz

formatNormal :: UTCTime -> String
formatNormal = formatTime defaultTimeLocale "%d . %m . %Y %R" .utcToLocalTime tz

formatClock :: UTCTime -> String
formatClock = formatTime defaultTimeLocale "%R" . utcToLocalTime tz

refTime :: UTCTime
refTime = UTCTime (fromGregorian 1970 1 1) 0

relativeDay :: Integer -> IO UTCTime
relativeDay offset = do
  UTCTime day _ <- getCurrentTime
  return $ UTCTime (addDays offset day) 0
