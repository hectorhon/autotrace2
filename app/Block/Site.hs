{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Block.Site where

import Servant
import Text.Blaze.Html5 hiding (head, map)
import Database.Persist.Postgresql
import Control.Monad.Trans.Either
import Control.Monad.Except
import Data.Text (Text)
import Data.ByteString.Char8 (unpack, pack)
import qualified Data.ByteString.Lazy.Char8 as LBS (unpack, pack)
import AppM
import Time
import Schema
import Common.Responses
import FileUpload
import Block.API
import Block.Views
import Block.Parse

blockSite :: ServerT BlockSite AppM
blockSite = toUploadBlockConfig
       :<|> uploadBlockConfig
       :<|> searchBlocks
       :<|> viewBlock

toUploadBlockConfig :: AppM Html
toUploadBlockConfig = (liftIO $ relativeDay (-1))
  >>= return . uploadBlockConfigPage

uploadBlockConfig :: MultiPartData Mem -> AppM Text
uploadBlockConfig (params, files) = runExceptT (do
  group <- maybe (throwError "Missing group")
                 (return . unpack)
                 (lookup "group" params)
  snapshotDate <- maybe (throwError "Missing snapshot date")
                        (return . localDayToUTC)
                        (lookup "snapshotdate" params
                         >>= return . unpack >>= parseDay)
  when (null files) (throwError "No file selected")
  case (parseBlocks $ pack $ LBS.unpack $ fileContent $ snd $ head files) of
    Left errMsg -> throwError (LBS.pack $ "Parse error: " ++ errMsg)
    Right blocks -> forM blocks $ \ (Block name type_ attrs) -> do
      mBlockHead <- runDb $ getBy (NameTypeGroup name type_ group)
      case mBlockHead of
        Nothing -> do
          bhid <- runDb $ insert $
                  BlockHead name type_ snapshotDate snapshotDate group
          runDb $ insertMany_ $
            map (uncurry $ BlockAttr bhid snapshotDate) attrs
        Just (Entity bhid _) -> do
          runDb $ update bhid [BlockHeadCurrentDate =. snapshotDate]
          runDb $ insertMany_ $
            map (uncurry $ BlockAttr bhid snapshotDate) attrs)
  >>= either (\ errMsg -> lift $ left $ err400 { errBody = errMsg })
             (\ _ -> redirect "/" >> return undefined)

searchBlocks = undefined
viewBlock = undefined
