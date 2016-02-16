module Job.Handlers where

import Servant
import Text.Blaze.Html5 hiding (select)
import Database.Esqueleto
import Job.Routes
import Job.Views
import Job.Types
import User.Types
import AppM

jobHandlers :: ServerT JobRoutes AppM
jobHandlers = viewJobs

viewJobs :: AppM Html
viewJobs = do
  jobs <- runDb $ select $ from $ \ (j `InnerJoin` u) -> do
    on (j ^. JobRecordScheduledBy ==. u ^. UserId)
    orderBy [desc (j ^. JobRecordScheduledOn), asc (j ^. JobRecordScheduledBy)]
    return (u, j)
  return (jobsPage jobs)
