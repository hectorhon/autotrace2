{-# LANGUAGE OverloadedStrings #-}

module Common.Responses where

import Servant
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Data.ByteString.Char8 (pack)
import AppM

redirect :: String -> AppM ()
redirect url = lift $ left $ err301 { errHeaders = [("location", pack url)] }
