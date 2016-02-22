{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Job.Routes where

import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5
import Data.Text
import Database.Persist.Postgresql
import User.RequireAuth
import Job.Types

type JobRoutes = ViewJobs
            :<|> CancelJob

type ViewJobs = "job" :> Get '[HTML] Html

type CancelJob = "job" :> Capture "jid" (Key JobRecord) :> "cancel"
                 :> RequireAuth SysAdminRole'
                 :> Post '[PlainText] Text
