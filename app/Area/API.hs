{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Area.API where

import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5
import Database.Persist.Postgresql
import Data.Text
import Schema
import User.RequireAuth

type AreaSite = ToCreateArea
           :<|> CreateArea
           :<|> ViewArea
           :<|> ViewAreas
           :<|> ToEditArea
           :<|> EditArea
           :<|> DeleteArea

type ToCreateArea = "area" :> "new"
                    :> QueryParam "parent" (Key Area)
                    :> RequireAuth WriteRole'
                    :> Get '[HTML] Html

type ToCreateArea' = "area" :> "new"
                     :> RequireAuth WriteRole'
                     :> Get '[HTML] Html

type CreateArea = "area" :> "new"
                  :> ReqBody '[FormUrlEncoded] Area
                  :> RequireAuth WriteRole'
                  :> Post '[PlainText] Text

type ViewArea = "area" :> Capture "aid" (Key Area)
                :> Get '[HTML] Html

type ViewAreas = "area" :> Get '[HTML] Html

type ToEditArea = "area" :> Capture "aid" (Key Area) :> "edit"
                  :> RequireAuth WriteRole'
                  :> Get '[HTML] Html

type EditArea = "area" :> Capture "aid" (Key Area) :> "edit"
                :> ReqBody '[FormUrlEncoded] Area
                :> RequireAuth WriteRole'
                :> Post '[PlainText] Text

type DeleteArea = "area" :> Capture "aid" (Key Area) :> "edit"
                  :> RequireAuth WriteRole'
                  :> Delete '[PlainText] Text
