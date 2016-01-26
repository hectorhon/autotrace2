{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Block.API where

import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5
import Database.Persist.Postgresql
import Data.Text
import FileUpload
import Schema

type BlockSite = ToUploadBlockConfig
            :<|> UploadBlockConfig
            :<|> SearchBlock
            :<|> ViewBlock

type ToUploadBlockConfig = "block" :> "upload" :> Get '[HTML] Html

type UploadBlockConfig = "block" :> "upload"
                         :> FilesMem
                         :> Post '[PlainText] Text

type SearchBlock = "block" :> "search"
                :> QueryParam "name" String
                :> QueryParam "type" String
                :> Get '[HTML] Html

type ViewBlock = "block" :> Capture "bid" (Key BlockHead)
               :> Get '[HTML] Html
