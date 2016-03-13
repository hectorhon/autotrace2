{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Apc.API where

import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5
import Database.Persist.Postgresql
import Data.Text
import Data.Time
import Area.Types
import Schema
import User.RequireAuth

type ApcSite = ToCreateApc
          :<|> CreateApc
          :<|> ViewApc
          :<|> ViewApcs
          :<|> ToEditApc
          :<|> EditApc
          :<|> DeleteApc
          :<|> ToCalculateApc
          :<|> CalculateApc
          :<|> IncrementalCalculateApc
          :<|> ViewApcPerformance

          :<|> ToCreateApcCv
          :<|> CreateApcCv
          :<|> ViewApcCv
          :<|> ToEditApcCv
          :<|> EditApcCv
          :<|> DeleteApcCv
          :<|> ViewApcCvTrend

type ToCreateApc = "area" :> Capture "aid" (Key Area) :> "apc" :> "new"
                   :> RequireAuth ControlAdminRole'
                   :> Get '[HTML] Html

type CreateApc = "area" :> Capture "aid" (Key Area) :> "apc" :> "new"
                 :> ReqBody '[FormUrlEncoded] Apc
                 :> RequireAuth ControlAdminRole'
                 :> Post '[PlainText] Text

type ViewApc = "area" :> Capture "aid" (Key Area)
               :> "apc" :> Capture "apcId" (Key Apc)
               :> Get '[HTML] Html

type ViewApcs = "apc" :> Get '[HTML] Html

type ToEditApc = "area" :> Capture "aid" (Key Area)
                 :> "apc" :> Capture "apcId" (Key Apc) :> "edit"
                 :> RequireAuth ControlAdminRole'
                 :> Get '[HTML] Html

type EditApc = "area" :> Capture "aid" (Key Area)
               :> "apc" :> Capture "apcId" (Key Apc) :> "edit"
               :> ReqBody '[FormUrlEncoded] Apc
               :> RequireAuth ControlAdminRole'
               :> Post '[PlainText] Text

type DeleteApc = "area" :> Capture "aid" (Key Area)
                 :> "apc" :> Capture "apcId" (Key Apc) :> "edit"
                 :> RequireAuth ControlAdminRole'
                 :> Delete '[PlainText] Text

type ToCalculateApc = "area" :> Capture "aid" (Key Area)
                      :> "apc" :> Capture "apcId" (Key Apc) :> "calculate"
                      :> QueryParam "start" Day :> QueryParam "end" Day
                      :> RequireAuth ControlAdminRole'
                      :> Get '[HTML] Html

type ToCalculateApc' = "area" :> Capture "aid" (Key Area)
                       :> "apc" :> Capture "apcId" (Key Apc) :> "calculate"
                       :> RequireAuth ControlAdminRole'
                       :> Get '[HTML] Html

type CalculateApc = "area" :> Capture "aid" (Key Area)
                    :> "apc" :> Capture "apcId" (Key Apc) :> "calculate"
                    :> ReqBody '[FormUrlEncoded] (Day, Day)
                    :> RequireAuth ControlAdminRole'
                    :> Post '[PlainText] Text

type IncrementalCalculateApc = "area" :> Capture "aid" (Key Area)
                               :> "apc" :> Capture "apcId" (Key Apc)
                               :> "icalculate"
                               :> RequireAuth ControlAdminRole'
                               :> Post '[PlainText] Text

type ViewApcPerformance = "area" :> Capture "aid" (Key Area)
                          :> "apc" :> Capture "apcId" (Key Apc) :> "performance"
                          :> QueryParam "start" Day :> QueryParam "end" Day
                          :> Get '[HTML] Html

type ViewApcPerformance' = "area" :> Capture "aid" (Key Area)
                          :> "apc" :> Capture "apcId" (Key Apc) :> "performance"
                          :> Get '[HTML] Html

type ToCreateApcCv = "area" :> Capture "aid" (Key Area)
                     :> "apc" :> Capture "apcId" (Key Apc)
                     :> "cv" :> "new"
                     :> RequireAuth ControlAdminRole'
                     :> Get '[HTML] Html

type CreateApcCv = "area" :> Capture "aid" (Key Area)
                   :> "apc" :> Capture "apcId" (Key Apc)
                   :> "cv" :> "new"
                   :> ReqBody '[FormUrlEncoded] Cv
                   :> RequireAuth ControlAdminRole'
                   :> Post '[PlainText] Text

type ViewApcCv = "area" :> Capture "aid" (Key Area)
                 :> "apc" :> Capture "apcId" (Key Apc)
                 :> "cv" :> Capture "cid" (Key Cv)
                 :> Get '[HTML] Html

type ToEditApcCv = "area" :> Capture "aid" (Key Area)
                   :> "apc" :> Capture "apcId" (Key Apc)
                   :> "cv" :> Capture "cid" (Key Cv) :> "edit"
                   :> RequireAuth ControlAdminRole'
                   :> Get '[HTML] Html

type EditApcCv = "area" :> Capture "aid" (Key Area)
                 :> "apc" :> Capture "apcId" (Key Apc)
                 :> "cv" :> Capture "cid" (Key Cv) :> "edit"
                 :> ReqBody '[FormUrlEncoded] Cv
                 :> RequireAuth ControlAdminRole'
                 :> Post '[PlainText] Text

type DeleteApcCv = "area" :> Capture "aid" (Key Area)
                   :> "apc" :> Capture "apcId" (Key Apc)
                   :> "cv" :> Capture "cid" (Key Cv) :> "edit"
                   :> RequireAuth ControlAdminRole'
                   :> Delete '[PlainText] Text

type ViewApcCvTrend = "area" :> Capture "aid" (Key Area)
                      :> "apc" :> Capture "apcId" (Key Apc)
                      :> "cv" :> Capture "cid" (Key Cv) :> "trend"
                      :> QueryParam "start" Day :> QueryParam "end" Day
                      :> Get '[HTML] Html

type ViewApcCvTrend' = "area" :> Capture "aid" (Key Area)
                       :> "apc" :> Capture "apcId" (Key Apc)
                       :> "cv" :> Capture "cid" (Key Cv) :> "trend"
                       :> Get '[HTML] Html
