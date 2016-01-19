module Area.Queries.IntervalsDuring where

import Database.Persist.Postgresql
import Area.Types
import TimeSeriesData
import Time
import Control.Monad

-- | Gets the (trimmed) intervals
intervalsDuring :: UTCTime -> UTCTime -> Key Area -> SqlPersistT IO [TSInterval]
intervalsDuring start end area =
  let timeFilter =
        [AreaIntervalStart >=. start, AreaIntervalStart <.  end] ||.
        [AreaIntervalEnd   >.  start, AreaIntervalEnd   <=. end] ||.
        [AreaIntervalStart <=. start, AreaIntervalEnd   >=. end]
  in liftM (map (trim . entityVal)) $
       selectList ( [ AreaIntervalArea ==. area ] ++ timeFilter )
                  [Asc AreaIntervalStart]
  where trim (AreaInterval _ start' end') =
          ( diffUTCTime (max start' start) refTime
          , diffUTCTime (min end' end) refTime )
