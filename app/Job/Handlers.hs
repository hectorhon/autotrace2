module Job.Handlers where

import Servant
import Text.Blaze.Html5 hiding (select)
import Data.Text
import Database.Esqueleto
import Job.Routes
import Job.Views
import Job.Types
import Job.Links
import User.Types
import Common.Responses
import AppM

jobHandlers :: ServerT JobRoutes AppM
jobHandlers = viewJobs
         :<|> cancelJob

viewJobs :: AppM Html
viewJobs = do
  jobs <- runDb $ select $ from $ \ (j `InnerJoin` u) -> do
    on (j ^. JobRecordScheduledBy ==. u ^. UserId)
    orderBy [desc (j ^. JobRecordScheduledOn), asc (j ^. JobRecordScheduledBy)]
    return (u, j)
  return (jobsPage jobs)

cancelJob :: Key JobRecord -> AppM Text
cancelJob jid = do
  runDb $ update $ \ j -> do set j [JobRecordCancelled =. (val True)]
                             where_ (j ^. JobRecordId ==. (val jid))
  redirect viewJobsLink'
  return undefined
