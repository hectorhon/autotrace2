{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Area.API where

import Servant
import Schema
import Servant.HTML.Blaze
import Text.Blaze.Html5
import Database.Persist.Postgresql

type AreaSite = ToCreateArea
           :<|> CreateArea
           :<|> ViewArea
           :<|> ViewAreas

type ToCreateArea = QueryParam "parent" (Key Area)
                    :> "area" :> "new"
                    :> Get '[HTML] Html

type ToCreateArea' = "area" :> "new"
                     :> Get '[HTML] Html

type CreateArea = ReqBody '[FormUrlEncoded] Area
                  :> "area"
                  :> Post '[HTML] Html

type ViewArea = "area" :> Capture "aid" (Key Area) :> "definition"
                :> Get '[HTML] Html

type ViewAreas = "area"
                 :> Get '[HTML] Html
