{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Block.Site where

import Servant
import Text.Blaze.Html5 hiding (head, map, i, select)
import Database.Esqueleto
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
          update $ \i -> where_ (i ^. BlockHeadCurrentDate ==. val snapshotDate)
          insertMany_ $ map (uncurry $ BlockAttr bhid snapshotDate) attrs)
  >>= either (\ errMsg -> lift $ left $ err400 { errBody = errMsg })
             (\ _ -> redirect "/" >> return undefined)

searchBlocks :: Maybe String -> Maybe String -> AppM Html
searchBlocks mName mType = do
  blocks <- case (mName, mType) of
    (Nothing, Nothing) -> return []
    _ -> let f = maybe "" id in runDb $ select $ from $ \ i -> do
      where_ (    (i ^. BlockHeadName  `ilike` ((%) ++. val (f mName) ++. (%)))
              &&. (i ^. BlockHeadType_ `ilike` ((%) ++. val (f mType) ++. (%))))
      orderBy [ asc (i ^. BlockHeadName) ]
      return i
  return (searchBlocksPage mName mType blocks)

viewBlock :: Key BlockHead -> AppM Html
viewBlock bid = do
  mBlockHead <- runDb (get bid)
  case mBlockHead of
    Nothing -> lift (left err404)
    Just blockHead -> do
      blockAttrs <- runDb $ select $ from $ \ i -> do
        where_ (i ^. BlockAttrBlock ==. val bid)
        orderBy [ asc (i ^. BlockAttrKey) ]
        return i
      return (viewBlockPage blockHead blockAttrs)
