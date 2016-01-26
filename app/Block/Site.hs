{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Block.Site where

import Servant
import Text.Blaze.Html5 hiding (head, map, i)
import Database.Persist.Postgresql
import qualified Database.Esqueleto as E
import Control.Monad.Trans.Either
import Control.Monad.Except
import Data.Text (Text)
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy.Char8 as LBS (pack)
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
toUploadBlockConfig = (liftIO $ relativeDay' (-1))
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
  case (parseBlocks $ fileContent $ snd $ head files) of
    Left errMsg -> throwError (LBS.pack $ "Parse error: " ++ errMsg)
    Right blocks -> runDb $ forM blocks $ \ (Block name type_ attrs) -> do
      mBlockHead <- getBy (NameTypeGroup name type_ group)
      case mBlockHead of
        Nothing -> do
          bhid <- insert $ BlockHead name type_ snapshotDate snapshotDate group
          insertMany_ $ map (uncurry $ BlockAttr bhid snapshotDate) attrs
        Just (Entity bhid _) -> do
          update bhid [BlockHeadCurrentDate =. snapshotDate]
          insertMany_ $ map (uncurry $ BlockAttr bhid snapshotDate) attrs)
  >>= either (\ errMsg -> lift $ left $ err400 { errBody = errMsg })
             (\ _ -> redirect "/" >> return undefined)

searchBlocks :: Maybe String -> AppM Html
searchBlocks Nothing = return $ searchBlocksPage Nothing []
searchBlocks (Just searchStr) = do
  blocks <- runDb $ E.select $ E.from $ \ i -> do
              E.where_ (i E.^. BlockHeadName `E.ilike`
                        ((E.%) E.++. E.val searchStr E.++. (E.%)))
              return i
  return $ searchBlocksPage (Just searchStr) blocks

viewBlock :: Key BlockHead -> AppM Html
viewBlock bid = do
  mBlockHead <- runDb (get bid)
  case mBlockHead of
    Nothing -> lift (left err404)
    Just blockHead -> do
      blockAttrs <- runDb $ selectList [BlockAttrBlock ==. bid]
                                       [Asc BlockAttrKey]
      return $ viewBlockPage blockHead blockAttrs
