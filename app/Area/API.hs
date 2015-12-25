{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Area.API where

import Servant
import Schema
import Servant.HTML.Blaze
import Text.Blaze.Html5
import Database.Persist.Postgresql
import Data.Text

type AreaSite = ToCreateArea
           :<|> CreateArea
           :<|> ViewArea
           :<|> ViewAreas
           :<|> UpdateArea
           :<|> DeleteArea

type ToCreateArea = QueryParam "parent" (Key Area)
                    :> "area" :> "new"
                    :> Get '[HTML] Html

type ToCreateArea' = "area" :> "new"
                     :> Get '[HTML] Html

type CreateArea = ReqBody '[FormUrlEncoded] Area
                  :> "area" :> "new"
                  :> Post '[PlainText] Text

type ViewArea = "area" :> Capture "aid" (Key Area) :> "definition"
                :> Get '[HTML] Html

type ViewAreas = "area"
                 :> Get '[HTML] Html

type UpdateArea = ReqBody '[FormUrlEncoded] Area
                  :> "area" :> Capture "aid" (Key Area) :> "definition"
                  :> Post '[PlainText] Text

type DeleteArea = "area" :> Capture "aid" (Key Area) :> "definition"
                  :> Delete '[PlainText] Text
