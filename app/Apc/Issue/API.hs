{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Apc.Issue.API where

import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5
import Database.Persist.Postgresql
import Data.Text
import Area.Types
import Schema

type ApcIssueSite = ToCreateApcIssue
               :<|> CreateApcIssue
               :<|> ViewApcIssue
               :<|> ViewApcIssues
               :<|> UpdateApcIssue
               :<|> DeleteApcIssue

type ToCreateApcIssue = "area" :> Capture "aid" (Key Area)
                        :> "apc" :> Capture "apcId" (Key Apc)
                        :> "issue" :> "new"
                        :> Get '[HTML] Html

type CreateApcIssue = "area" :> Capture "aid" (Key Area)
                      :> "apc" :> Capture "apcId" (Key Apc)
                      :> "issue" :> "new"
                      :> ReqBody '[FormUrlEncoded] ApcIssue
                      :> Post '[PlainText] Text

type ViewApcIssue = "area" :> Capture "aid" (Key Area)
                    :> "apc" :> Capture "apcId" (Key Apc)
                    :> "issue" :> Capture "iid" (Key ApcIssue) :> "overview"
                    :> Get '[HTML] Html

type ViewApcIssues = "area" :> Capture "aid" (Key Area)
                    :> "apc" :> Capture "apcId" (Key Apc)
                    :> "issue" :> "list"
                    :> Get '[HTML] Html

type UpdateApcIssue = "area" :> Capture "aid" (Key Area)
                      :> "apc" :> Capture "apcId" (Key Apc)
                      :> "issue" :> Capture "iid" (Key ApcIssue) :> "overview"
                      :> ReqBody '[FormUrlEncoded] ApcIssue
                      :> Post '[PlainText] Text

type DeleteApcIssue = "area" :> Capture "aid" (Key Area)
                      :> "apc" :> Capture "apcId" (Key Apc)
                      :> "issue" :> Capture "iid" (Key ApcIssue) :> "overview"
                      :> Delete '[PlainText] Text
