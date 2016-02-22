{-# LANGUAGE OverloadedStrings #-}

module Job.Views where

import Text.Blaze.Html5 as H hiding (map, i)
import Text.Blaze.Html5.Attributes as Ha
import Control.Monad (forM_)
import Database.Persist.Postgresql
import Job.Types
import Job.Links
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
      th "Cancel"
    forM_ (zip (map show ([1..]::[Int])) jobs) $
      \ (i, (Entity _ user, Entity jid job)) -> do
        tr $ do
          td $ toHtml $ jobRecordDescription job
          td $ toHtml $ userName user
          td $ toHtml $ formatNormal (jobRecordScheduledOn job)
          td $ toHtml $ jobRecordProgress job
          td $ if not (jobRecordCancelled job) && (jobRecordProgress job) == 0
               then a ! Ha.id (stringValue $ "cancel-job-link-" ++ i)
                      ! href "#" $ "[X]"
               else ""
          script $ toHtml $
            " $('#cancel-job-link-"++ i ++"').click(function(e) {             \
            \   e.preventDefault();                                           \
            \   $.post('" ++ (cancelJobLink' jid) ++ "').success(function() { \
            \     window.location.href = '" ++ viewJobsLink' ++ "';           \
            \   });                                                           \
            \ });                                                             "
