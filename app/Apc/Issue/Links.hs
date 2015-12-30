module Apc.Issue.Links where

import Servant
import Database.Persist.Postgresql
import Text.Blaze.Html5
import Common.Links
import Apc.Issue.API
import Schema

toCreateApcIssueLink :: Key Area -> Key Apc -> AttributeValue
toCreateApcIssueLink aid apcId = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ToCreateApcIssue) aid apcId)



viewApcIssueLink' :: Key Area -> Key Apc -> Key ApcIssue -> String
viewApcIssueLink' aid apcid iid =
  "/" ++ show (linkTo (Proxy :: Proxy ViewApcIssue) aid apcid iid)

viewApcIssueLink :: Key Area -> Key Apc -> Key ApcIssue -> AttributeValue
viewApcIssueLink aid apcid iid = stringValue $ viewApcIssueLink' aid apcid iid



viewApcIssuesLink' :: Key Area -> Key Apc -> String
viewApcIssuesLink' aid apcid =
  "/" ++ show (linkTo (Proxy :: Proxy ViewApcIssues) aid apcid)

viewApcIssuesLink :: Key Area -> Key Apc -> AttributeValue
viewApcIssuesLink aid apcid = stringValue $ viewApcIssuesLink' aid apcid
