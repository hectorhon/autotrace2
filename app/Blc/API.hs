{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Blc.API where

import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5
import Database.Persist.Postgresql
import Data.Text
import Area.Types
import Blc.Types
import Time
import User.RequireAuth
import User.GetUser

type BlcSite = ToCreateBlc
          :<|> CreateBlc
          :<|> ViewBlc
          :<|> ToEditBlc
          :<|> EditBlc
          :<|> DeleteBlc

          :<|> ToCalculateBlc
          :<|> CalculateBlc
          :<|> ViewBlcsPerformance
          :<|> ViewBlcBadActors
          :<|> ToCalculateAreaBlcs
          :<|> CalculateAreaBlcs

          :<|> ListBlcTags

          :<|> ToCreateBlcLabel
          :<|> CreateBlcLabel
          :<|> ViewBlcLabel
          :<|> ViewBlcLabels
          :<|> ToEditBlcLabel
          :<|> EditBlcLabel
          :<|> DeleteBlcLabel
          :<|> LabelBlc
          :<|> UnlabelBlc

type ToCreateBlc = "area" :> Capture "aid" (Key Area) :> "blc" :> "new"
                   :> RequireAuth ControlAdminRole'
                   :> Get '[HTML] Html

type CreateBlc = "area" :> Capture "aid" (Key Area) :> "blc" :> "new"
                 :> ReqBody '[FormUrlEncoded] Blc
                 :> RequireAuth ControlAdminRole'
                 :> Post '[PlainText] Text

type ViewBlc = "area" :> Capture "aid" (Key Area)
               :> "blc" :> Capture "bid" (Key Blc)
               :> Get '[HTML] Html

type ToEditBlc = "area" :> Capture "aid" (Key Area)
                 :> "blc" :> Capture "bid" (Key Blc) :> "edit"
                 :> RequireAuth ControlAdminRole'
                 :> Get '[HTML] Html

type EditBlc = "area" :> Capture "aid" (Key Area)
               :> "blc" :> Capture "bid" (Key Blc) :> "edit"
               :> ReqBody '[FormUrlEncoded] Blc
               :> RequireAuth ControlAdminRole'
               :> Post '[PlainText] Text

type DeleteBlc = "area" :> Capture "aid" (Key Area)
               :> "blc" :> Capture "bid" (Key Blc) :> "edit"
               :> RequireAuth ControlAdminRole'
               :> Delete '[PlainText] Text

type ToCalculateBlc = "area" :> Capture "aid" (Key Area)
                      :> "blc" :> Capture "bid" (Key Blc) :> "calculate"
                      :> QueryParam "start" Day :> QueryParam "end" Day
                      :> RequireAuth ControlAdminRole'
                      :> Get '[HTML] Html

type ToCalculateBlc' = "area" :> Capture "aid" (Key Area)
                       :> "blc" :> Capture "bid" (Key Blc) :> "calculate"
                       :> RequireAuth ControlAdminRole'
                       :> Get '[HTML] Html

type CalculateBlc = "area" :> Capture "aid" (Key Area)
                    :> "blc" :> Capture "bid" (Key Blc) :> "calculate"
                    :> ReqBody '[FormUrlEncoded] (Day, Day)
                    :> RequireAuth ControlAdminRole'
                    :> GetUser
                    :> Post '[PlainText] Text

type ViewBlcsPerformance = "area" :> Capture "aid" (Key Area)
                           :> "blc" :> "performance"
                           :> QueryParam "start" Day :> QueryParam "end" Day
                           :> Get '[HTML] Html

type ViewBlcBadActors = "area" :> Capture "aid" (Key Area)
                        :> "blc" :> "bad-actors"
                        :> QueryParam "start" Day :> QueryParam "end" Day
                        :> QueryParam "complianceTargetPct" Double
                        :> QueryParam "qualityTargetPct" Double
                        :> Get '[HTML] Html

type ViewBlcsPerformance' = "area" :> Capture "aid" (Key Area)
                            :> "blc" :> "performance"
                            :> Get '[HTML] Html

type ToCalculateAreaBlcs = "area" :> Capture "aid" (Key Area)
                           :> "blc" :> "calculate"
                           :> QueryParam "start" Day :> QueryParam "end" Day
                           :> RequireAuth ControlAdminRole'
                           :> Get '[HTML] Html

type CalculateAreaBlcs = "area" :> Capture "aid" (Key Area)
                         :> "blc" :> "calculate"
                         :> ReqBody '[FormUrlEncoded] (Day, Day)
                         :> RequireAuth ControlAdminRole'
                         :> GetUser
                         :> Post '[PlainText] Text

type ListBlcTags = "area" :> Capture "aid" (Key Area)
                   :> "blc" :> "tags"
                   :> Get '[HTML] Html

type ToCreateBlcLabel = "blc" :> "label" :> "new"
                        :> RequireAuth ControlAdminRole'
                        :> Get '[HTML] Html

type CreateBlcLabel = "blc" :> "label" :> "new"
                      :> ReqBody '[FormUrlEncoded] BlcLabel
                      :> RequireAuth ControlAdminRole'
                      :> Post '[PlainText] Text

type ViewBlcLabel = "blc" :> "label" :> Capture "lid" (Key BlcLabel)
                    :> Get '[HTML] Html

type ViewBlcLabels = "blc" :> "label" :> Get '[HTML] Html

type ToEditBlcLabel = "blc" :> "label" :> Capture "lid" (Key BlcLabel) :> "edit"
                      :> RequireAuth ControlAdminRole'
                      :> Get '[HTML] Html

type EditBlcLabel = "blc" :> "label" :> Capture "lid" (Key BlcLabel) :> "edit"
                    :> ReqBody '[FormUrlEncoded] BlcLabel
                    :> RequireAuth ControlAdminRole'
                    :> Post '[PlainText] Text

type DeleteBlcLabel = "blc" :> "label" :> Capture "lid" (Key BlcLabel)
                      :> RequireAuth ControlAdminRole'
                      :> Delete '[PlainText] Text

type LabelBlc = "blc" :> Capture "bid" (Key Blc) :> "label"
                :> ReqBody '[PlainText] Text
                :> RequireAuth ControlAdminRole'
                :> Put '[PlainText] Text

type UnlabelBlc = "blc" :> Capture "bid" (Key Blc)
                  :> "label" :> Capture "label" String
                  :> RequireAuth ControlAdminRole'
                  :> Delete '[PlainText] Text
