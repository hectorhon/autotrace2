{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Apc.API where

import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5
import Database.Persist.Postgresql
import Data.Text
import Schema

type ApcSite = ToCreateApc
          :<|> CreateApc
          :<|> ViewApc
          :<|> UpdateApc
          :<|> DeleteApc
          :<|> ToCreateApcCv
          :<|> CreateApcCv
          :<|> ViewApcCv
          :<|> UpdateApcCv
          :<|> DeleteApcCv

type ToCreateApc = "area" :> Capture "aid" (Key Area) :> "apc" :> "new"
                   :> Get '[HTML] Html

type CreateApc = "area" :> Capture "aid" (Key Area) :> "apc" :> "new"
                 :> ReqBody '[FormUrlEncoded] Apc
                 :> Post '[PlainText] Text

type ViewApc = "area" :> Capture "aid" (Key Area)
               :> "apc" :> Capture "apcId" (Key Apc) :> "definition"
               :> Get '[HTML] Html

type UpdateApc = "area" :> Capture "aid" (Key Area)
                 :> "apc" :> Capture "apcId" (Key Apc) :> "definition"
                 :> ReqBody '[FormUrlEncoded] Apc
                 :> Post '[PlainText] Text

type DeleteApc = "area" :> Capture "aid" (Key Area)
                 :> "apc" :> Capture "apcId" (Key Apc) :> "definition"
                 :> Delete '[PlainText] Text

type ToCreateApcCv = "area" :> Capture "aid" (Key Area)
                     :> "apc" :> Capture "apcId" (Key Apc)
                     :> "cv" :> "new"
                     :> Get '[HTML] Html

type CreateApcCv = "area" :> Capture "aid" (Key Area)
                   :> "apc" :> Capture "apcId" (Key Apc)
                   :> "cv" :> "new"
                   :> ReqBody '[FormUrlEncoded] Cv
                   :> Post '[PlainText] Text

type ViewApcCv = "area" :> Capture "aid" (Key Area)
                 :> "apc" :> Capture "apcId" (Key Apc)
                 :> "cv" :> Capture "cid" (Key Cv) :> "definition"
                 :> Get '[HTML] Html

type UpdateApcCv = "area" :> Capture "aid" (Key Area)
                   :> "apc" :> Capture "apcId" (Key Apc)
                   :> "cv" :> Capture "cid" (Key Cv) :> "definition"
                   :> ReqBody '[FormUrlEncoded] Cv
                   :> Post '[PlainText] Text

type DeleteApcCv = "area" :> Capture "aid" (Key Area)
                   :> "apc" :> Capture "apcId" (Key Apc)
                   :> "cv" :> Capture "cid" (Key Cv) :> "definition"
                   :> Delete '[PlainText] Text
