{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Area.API where

import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5
import Database.Persist.Postgresql
import Data.Text
import Schema

type AreaSite = ToCreateArea
           :<|> CreateArea
           :<|> ViewArea
           :<|> ViewAreas
           :<|> UpdateArea
           :<|> DeleteArea

type ToCreateArea = "area" :> "new"
                    :> QueryParam "parent" (Key Area)
                    :> Get '[HTML] Html

type ToCreateArea' = "area" :> "new"
                     :> Get '[HTML] Html

type CreateArea = "area" :> "new"
                  :> ReqBody '[FormUrlEncoded] Area
                  :> Post '[PlainText] Text

type ViewArea = "area" :> Capture "aid" (Key Area) :> "definition"
                :> Get '[HTML] Html

type ViewAreas = "area"
                 :> Get '[HTML] Html

type UpdateArea = "area" :> Capture "aid" (Key Area) :> "definition"
                  :> ReqBody '[FormUrlEncoded] Area
                  :> Post '[PlainText] Text

type DeleteArea = "area" :> Capture "aid" (Key Area) :> "definition"
                  :> Delete '[PlainText] Text
