{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lopc.Routes where

import Servant
import Servant.HTML.Blaze
import Database.Persist.Postgresql
import Text.Blaze.Html5
import Data.Text
import Lopc.Types
import User.RequireAuth

type LopcRoutes = ViewLopcOverview
             :<|> ViewLopcList
             :<|> ToCreateLopc
             :<|> CreateLopc
             :<|> ViewLopc
             :<|> ToEditLopc
             :<|> EditLopc
             :<|> DeleteLopc

type ViewLopcOverview = "lopc"
                      :> QueryParam "year" Integer
                      :> QueryParam "hazardous" Bool
                      :> Get '[HTML] Html
type ViewLopcOverview' = "lopc" :> Get '[HTML] Html

type ViewLopcList = "lopc" :> "list" :> QueryParam "status" Text
                     :> Get '[HTML] Html
type ViewLopcList' = "lopc" :> "list" :> Get '[HTML] Html

type ToCreateLopc = "lopc" :> "new"
                    :> RequireAuth LopcUserRole'
                    :> Get '[HTML] Html

type CreateLopc = "lopc" :> "new"
                  :> ReqBody '[FormUrlEncoded] Lopc
                  :> RequireAuth LopcUserRole'
                  :> Post '[PlainText] Text

type ViewLopc = "lopc" :> Capture "lid" (Key Lopc) :> Get '[HTML] Html

type ToEditLopc = "lopc" :> Capture "lid" (Key Lopc) :> "edit"
                  :> RequireAuth LopcUserRole'
                  :> Get '[HTML] Html

type EditLopc = "lopc" :> Capture "lid" (Key Lopc) :> "edit"
                :> ReqBody '[FormUrlEncoded] Lopc
                :> RequireAuth LopcUserRole'
                :> Post '[PlainText] Text

type DeleteLopc = "lopc" :> Capture "lid" (Key Lopc) :> "edit"
                  :> RequireAuth LopcAdminRole'
                  :> Delete '[PlainText] Text
