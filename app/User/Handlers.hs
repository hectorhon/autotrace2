{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module User.Handlers where

import Servant
import Text.Blaze.Html5 hiding (map)
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
          :<|> toResetPassword
          :<|> resetPassword
          :<|> deleteUser
          :<|> toAssignRole
          :<|> assignRole
          :<|> deleteRole

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

viewUsers :: AppM Html
viewUsers = do
  users <- runDb (selectList [UserHash !=. ""] [Asc UserName])
  return (usersPage users)

toCreateUser :: AppM Html
toCreateUser = return userNewPage

createUser :: LoginData -> AppM Text
createUser (LoginData name pass) = do
  hash <- liftIO (makePassword pass 17)
  mUid <- runDb (insertUnique (User name hash))
  case mUid of
    Nothing -> lift $ left $ err400 { errBody = "Username is already taken" }
    Just uid -> redirect (viewUserLink' uid) >> return undefined

viewUser :: Key User -> AppM Html
viewUser uid = do
  mUser <- runDb (get uid)
  case mUser of
    Nothing -> lift (left err404)
    Just user -> do
      roles <- runDb (selectList [RoleUser ==. uid] [Asc RoleRole])
      return (userPage (Entity uid user) roles)

toResetPassword :: Key User -> AppM Html
toResetPassword uid =
  runDb (selectList [UserId ==. uid] [])
  >>= maybe (lift (left err404)) (return . resetPasswordPage) . listToMaybe

resetPassword :: Key User -> LoginData -> AppM Text
resetPassword uid (LoginData _ pass) = do
  hash <- liftIO (makePassword pass 17)
  runDb (update uid [UserHash =. hash])
  redirect (viewUserLink' uid)
  return undefined

deleteUser :: Key User -> AppM Text
deleteUser uid = do
  runDb (update uid [UserHash =. ""])
  redirect viewUsersLink'
  return "deactivated"

toAssignRole :: Key User -> AppM Html
toAssignRole uid = do
  mUser <- runDb (get uid)
  case mUser of Nothing -> lift (left err404)
                Just user -> return (assignRolePage (Entity uid user))

assignRole :: Key User -> Role -> AppM Text
assignRole _ role = do
  mRid <- runDb (insertUnique role)
  case mRid of
    Nothing -> lift $ left $ err400 { errBody = "Duplicate user role" }
    Just _ -> redirect (viewUserLink' (roleUser role)) >> return undefined

deleteRole :: Key User -> Key Role -> AppM Text
deleteRole _ rid = runDb (delete rid) >> return "deleted"
