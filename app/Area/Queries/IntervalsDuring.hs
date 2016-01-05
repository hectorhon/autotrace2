module Area.Queries.IntervalsDuring where

import Database.Persist.Postgresql
import Schema
import TimeSeriesData
import Time
import Control.Monad

-- | Gets the (trimmed) intervals
intervalsDuring :: UTCTime -> UTCTime -> MetricType -> Key Area
                -> SqlPersistT IO [TSInterval]
intervalsDuring start end metricType area =
  let timeFilter =
        [AreaIntervalStart >=. start, AreaIntervalStart <.  end] ||.
        [AreaIntervalEnd   >.  start, AreaIntervalEnd   <=. end] ||.
        [AreaIntervalStart <=. start, AreaIntervalEnd   >=. end]
  in liftM (map (trim . entityVal)) $
       selectList ( [ AreaIntervalArea ==. area
                    , AreaIntervalCategory ==. metricType
                    ] ++ timeFilter ) [Asc AreaIntervalStart]
  where trim (AreaInterval _ start' end' _) =
          ( diffUTCTime (max start' start) refTime
          , diffUTCTime (min end' end) refTime )
