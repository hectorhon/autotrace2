{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Job.Routes where

import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5

type JobRoutes = ViewJobs

type ViewJobs = "job" :> Get '[HTML] Html
