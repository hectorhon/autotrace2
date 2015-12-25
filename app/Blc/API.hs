{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Blc.API where

import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5
import Database.Persist.Postgresql
import Data.Text
import Schema

type BlcSite = ToCreateBlc
          :<|> CreateBlc
          :<|> ViewBlc
          :<|> UpdateBlc
          :<|> DeleteBlc

type ToCreateBlc = "area" :> Capture "aid" (Key Area)
                   :> "blc" :> "new"
                   :> Get '[HTML] Html

type CreateBlc = ReqBody '[FormUrlEncoded] Area
                 :> "area" :> Capture "aid" (Key Area)
                 :> "blc" :> "new"
                 :> Post '[PlainText] Text

type ViewBlc = "area" :> Capture "aid" (Key Area)
               :> "blc" :> Capture "bid" (Key Blc) :> "definition"
               :> Get '[HTML] Html

type UpdateBlc = "area" :> Capture "aid" (Key Area)
               :> "blc" :> Capture "bid" (Key Blc) :> "definition"
               :> Post '[PlainText] Text

type DeleteBlc = "area" :> Capture "aid" (Key Area)
               :> "blc" :> Capture "bid" (Key Blc) :> "definition"
               :> Delete '[PlainText] Text
