{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Blc.API where

import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5
import Database.Persist.Postgresql
import Data.Text
import Schema
import Time

type BlcSite = ToCreateBlc
          :<|> CreateBlc
          :<|> ViewBlc
          :<|> UpdateBlc
          :<|> DeleteBlc
          :<|> ToCalculateBlc

type ToCreateBlc = "area" :> Capture "aid" (Key Area)
                   :> "blc" :> "new"
                   :> Get '[HTML] Html

type CreateBlc = "area" :> Capture "aid" (Key Area) :> "blc" :> "new"
                 :> ReqBody '[FormUrlEncoded] Blc
                 :> Post '[PlainText] Text

type ViewBlc = "area" :> Capture "aid" (Key Area)
               :> "blc" :> Capture "bid" (Key Blc) :> "definition"
               :> Get '[HTML] Html

type UpdateBlc = "area" :> Capture "aid" (Key Area)
                 :> "blc" :> Capture "bid" (Key Blc) :> "definition"
                 :> ReqBody '[FormUrlEncoded] Blc
                 :> Post '[PlainText] Text

type DeleteBlc = "area" :> Capture "aid" (Key Area)
               :> "blc" :> Capture "bid" (Key Blc) :> "definition"
               :> Delete '[PlainText] Text

type ToCalculateBlc = "area" :> Capture "aid" (Key Area)
                      :> "blc" :> Capture "bid" (Key Blc) :> "calculate"
                      :> QueryParam "start" Day :> QueryParam "end" Day
                      :> Get '[HTML] Html

type ToCalculateBlc' = "area" :> Capture "aid" (Key Area)
                       :> "blc" :> Capture "bid" (Key Blc) :> "calculate"
                       :> Get '[HTML] Html
