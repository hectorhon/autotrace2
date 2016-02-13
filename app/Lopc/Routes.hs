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

type LopcRoutes = ToCreateLopc
             :<|> CreateLopc
             :<|> ViewLopcs
             :<|> ViewLopcsOverview
             :<|> ViewLopc
             :<|> ToEditLopc
             :<|> EditLopc
             :<|> DeleteLopc

type ToCreateLopc = "lopc" :> "new"
                    :> RequireAuth LopcUserRole'
                    :> Get '[HTML] Html

type CreateLopc = "lopc" :> "new"
                  :> ReqBody '[FormUrlEncoded] Lopc
                  :> RequireAuth LopcUserRole'
                  :> Post '[PlainText] Text

type ViewLopcs = "lopc" :> "summary" :> QueryParam "year" Integer
                 :> Get '[HTML] Html
type ViewLopcs' = "lopc" :> "summary" :> Get '[HTML] Html

type ViewLopcsOverview = "lopc" :> "overview" :> QueryParam "year" Integer
                         :> Get '[HTML] Html
type ViewLopcsOverview' = "lopc" :> "overview" :> Get '[HTML] Html

type ViewLopc = "lopc" :> Capture "lid" (Key Lopc) :> Get '[HTML] Html

type ToEditLopc = "lopc" :> Capture "lid" (Key Lopc) :> "edit"
                  :> RequireAuth LopcUserRole'
                  :> Get '[HTML] Html

type EditLopc = "lopc" :> Capture "lid" (Key Lopc) :> "edit"
                :> ReqBody '[FormUrlEncoded] Lopc
                :> RequireAuth LopcUserRole'
                :> Post '[PlainText] Text

type DeleteLopc = "lopc" :> Capture "lid" (Key Lopc) :> "edit"
                  :> Delete '[PlainText] Text
