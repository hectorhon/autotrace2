{-# LANGUAGE OverloadedStrings #-}

module Job.Views where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as Ha
import Control.Monad (forM_)
import Database.Persist.Postgresql
import Job.Types
import User.Types
import Common.Views
import Time

jobsPage :: [(Entity User, Entity JobRecord)] -> Html
jobsPage jobs = layout "Jobs history" $ do
  h1 "Jobs history"
  table ! class_ "list-table" $ do
    tr $ do
      th "Description"
      th "Scheduled by"
      th "Scheduled on"
      th "Progress"
    forM_ jobs $ \ (Entity _ user, Entity _ job) -> do
      tr $ do
        td $ toHtml $ jobRecordDescription job
        td $ toHtml $ userName user
        td $ toHtml $ formatNormal (jobRecordScheduledOn job)
        td $ toHtml $ jobRecordProgress job
