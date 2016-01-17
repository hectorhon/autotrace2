{-# LANGUAGE OverloadedStrings #-}

module Common.PackErrMiddleware
  ( packErr
  ) where

import Network.Wai
import Network.HTTP.Types
import Blaze.ByteString.Builder (toByteString, fromLazyByteString)
import Data.ByteString.Char8 as B (unpack, null, head, tail, snoc)
import Data.Char (toUpper)
import Text.Blaze.Html5 as H hiding (body, head)
import Text.Blaze.Html5.Attributes as Ha hiding (headers)
import Text.Blaze.Html.Renderer.Utf8
import Common.Views

packErr :: Middleware
packErr app rq k = app rq $ \ res ->
  let (status, headers, withBody) = responseToStream res
      sendBody body = k $ responseStream status headers $ \ write flush -> do
        (flip body) undefined $ \ builder ->
          let msg' = toByteString builder
              msg = if B.null msg' then ""
                    else (toUpper $ B.head msg')
                         : unpack (B.tail (snoc msg' '.'))
          in write (fromLazyByteString $ renderHtml $ errPage msg) >> flush
  in if statusCode status >= 400 then withBody sendBody else k res

errPage :: String -> Html
errPage msg = layoutNoBanner "Error" $ do
  h1 "Oops..."
  p (toHtml msg)
  a ! Ha.id "back-link" ! href "#" $ "Go back"
  script $ " $('#back-link').click(function() {             \
           \   history.go(-1);                              \
           \ });                                            "
