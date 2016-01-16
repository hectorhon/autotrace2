{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module User.Handlers where

import Servant
import Text.Blaze.Html5
import Database.Persist.Postgresql
import Crypto.PasswordStore
import System.Random
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.Maybe
import Data.Time
import Data.Text (Text)
import Data.ByteString.Char8 (pack)
import Blaze.ByteString.Builder (toByteString)
import User.Types
import User.Routes
import User.Views
import User.Links
import Home.Links
import Common.Responses
import AppM
import Web.Cookie

userHandlers :: ServerT UserRoutes AppM
userHandlers = toLogin
          :<|> login
          :<|> logout

toLogin :: AppM Html
toLogin = return loginPage

login :: LoginData -> AppM Text
login (LoginData name pass) = do
  mUser <- runDb $ selectFirst [UserName ==. name] []
  case mUser of
    Nothing -> redirect toLoginLink' >> return undefined
    Just (Entity uid user) -> do
      if verifyPassword pass (userHash user)
      then do
        ident <- fmap pack $ liftIO $
                 sequence $ replicate 32 $ randomRIO ('a', 'z')
        loginTime <- liftIO getCurrentTime
        runDb $ insert_ (Session ident uid loginTime)
        let usernameCookie = toByteString $ renderSetCookie $
              def { setCookieName = "username"
                  , setCookieValue = pack $ userName user }
        let identCookie = toByteString $ renderSetCookie $
              def { setCookieName = "ident" , setCookieValue = ident }
        _ <- lift $ left $
             err301 { errHeaders = [ ("location"  , pack homePageLink')
                                   , ("set-cookie", usernameCookie    )
                                   , ("set-cookie", identCookie       ) ] }
        return undefined
      else redirect toLoginLink' >> return undefined

logout :: Maybe String -> AppM Text
logout mIdent = do
  when (isJust mIdent) $
    runDb $ deleteWhere [SessionIdent ==. pack (fromJust mIdent)]
  let usernameCookie = toByteString $ renderSetCookie $
        def { setCookieName = "username" , setCookieValue = "none" }
  let identCookie = toByteString $ renderSetCookie $
        def { setCookieName = "ident" , setCookieValue = "none" }
  _ <- lift $ left $
       err301 { errHeaders = [ ("location"  , pack toLoginLink')
                             , ("set-cookie", usernameCookie    )
                             , ("set-cookie", identCookie       ) ] }
  return undefined
