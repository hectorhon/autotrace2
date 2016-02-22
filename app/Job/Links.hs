{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Job.Links where

import Servant
import Database.Persist.Postgresql
import Job.Routes
import Job.Types
import Common.Links

viewJobsLink' :: String
viewJobsLink' = "/" ++ show (linkTo (Proxy :: Proxy ViewJobs))

cancelJobLink' :: Key JobRecord -> String
cancelJobLink' jid = "/" ++ show (linkTo (Proxy :: Proxy CancelJob) jid)
