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
          :<|> viewUsers
          :<|> toCreateUser
          :<|> createUser
          :<|> viewUser
          :<|> toEditUser
          :<|> editUser
          :<|> assignRole
          :<|> deleteRole
          :<|> deleteUser

toLogin :: Maybe Bool -> AppM Html
toLogin mValid = let isValid = maybe True id mValid in
  return (loginPage isValid)

login :: LoginData -> AppM Text
login (LoginData name pass) = do
  mUser <- runDb $ selectFirst [UserName ==. name] []
  case mUser of
    Nothing -> redirect (toLoginLink' False) >> return undefined
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
             err303 { errHeaders = [ ("location"  , pack homePageLink')
                                   , ("set-cookie", usernameCookie    )
                                   , ("set-cookie", identCookie       ) ] }
        return undefined
      else redirect (toLoginLink' False) >> return undefined

logout :: Maybe String -> AppM Text
logout mCookie = do
  when (isJust mCookie) $
    let cookies = parseCookies (pack $ fromJust mCookie)
        mIdent = lookup "ident" cookies
    in case mIdent of
         Nothing -> return ()
         Just ident -> runDb $ deleteWhere [SessionIdent ==. ident]
  let usernameCookie = toByteString $ renderSetCookie $
        def { setCookieName = "username" , setCookieValue = "guest" }
  let identCookie = toByteString $ renderSetCookie $
        def { setCookieName = "ident" , setCookieValue = "guest" }
  _ <- lift $ left $
       err303 { errHeaders = [ ("location"  , pack $ toLoginLink' True)
                             , ("set-cookie", usernameCookie    )
                             , ("set-cookie", identCookie       ) ] }
  return undefined

viewUsers = undefined
toCreateUser = undefined
createUser = undefined
viewUser = undefined
toEditUser = undefined
editUser = undefined
assignRole = undefined
deleteRole = undefined
deleteUser = undefined
