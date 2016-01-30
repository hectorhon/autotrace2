{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Area.API where

import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5
import Database.Persist.Postgresql
import Data.Text
import Area.Types
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
                    :> RequireAuth AreaAdminRole'
                    :> Get '[HTML] Html

type ToCreateArea' = "area" :> "new"
                     :> RequireAuth AreaAdminRole'
                     :> Get '[HTML] Html

type CreateArea = "area" :> "new"
                  :> ReqBody '[FormUrlEncoded] Area
                  :> RequireAuth AreaAdminRole'
                  :> Post '[PlainText] Text

type ViewArea = "area" :> Capture "aid" (Key Area)
                :> Get '[HTML] Html

type ViewAreas = "area" :> QueryParam "browse" String :> Get '[HTML] Html

type ToEditArea = "area" :> Capture "aid" (Key Area) :> "edit"
                  :> RequireAuth AreaAdminRole'
                  :> Get '[HTML] Html

type EditArea = "area" :> Capture "aid" (Key Area) :> "edit"
                :> ReqBody '[FormUrlEncoded] Area
                :> RequireAuth AreaAdminRole'
                :> Post '[PlainText] Text

type DeleteArea = "area" :> Capture "aid" (Key Area) :> "edit"
                  :> RequireAuth AreaAdminRole'
                  :> Delete '[PlainText] Text
