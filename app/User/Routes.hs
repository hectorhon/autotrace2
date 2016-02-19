{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module User.Routes where

import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5
import Database.Persist.Postgresql
import Data.Text
import User.Types
import User.RequireAuth

type UserRoutes = ToLogin
             :<|> Login
             :<|> Logout
             :<|> ViewUsers
             :<|> ToCreateUser
             :<|> CreateUser
             :<|> ViewUser
             :<|> ToResetPassword
             :<|> ResetPassword
             :<|> DeleteUser
             :<|> ToAssignRole
             :<|> AssignRole
             :<|> DeleteRole

type ToLogin = "login" :> QueryParam "valid" Bool :> Get '[HTML] Html

type Login = "login"
             :> ReqBody '[FormUrlEncoded] LoginData
             :> Post '[PlainText] Text

type Logout = "logout" :> Header "Cookie" String :> Post '[PlainText] Text

type ViewUsers = "users"
               :> RequireAuth ManageUsersRole'
               :> Get '[HTML] Html

type ToCreateUser = "users" :> "new"
                    :> RequireAuth ManageUsersRole'
                    :> Get '[HTML] Html

type CreateUser = "users" :> "new"
                  :> ReqBody '[FormUrlEncoded] LoginData
                  :> RequireAuth ManageUsersRole'
                  :> Post '[HTML] Text

type ViewUser = "users" :> Capture "uid" (Key User)
                :> RequireAuth ManageUsersRole'
                :> Get '[HTML] Html

type ToResetPassword = "users" :> Capture "uid" (Key User) :> "reset"
                       :> RequireAuth ManageUsersRole'
                       :> Get '[HTML] Html

type ResetPassword = "users" :> Capture "uid" (Key User) :> "reset"
                     :> ReqBody '[FormUrlEncoded] LoginData
                     :> RequireAuth ManageUsersRole'
                     :> Post '[HTML] Text

type DeleteUser = "users" :> Capture "uid" (Key User)
                  :> RequireAuth ManageUsersRole'
                  :> Delete '[HTML] Text

type ToAssignRole = "users" :> Capture "uid" (Key User) :> "roles" :> "new"
                    :> Get '[HTML] Html

type AssignRole = "users" :> Capture "uid" (Key User) :> "roles" :> "new"
                  :> ReqBody '[FormUrlEncoded] Role
                  :> RequireAuth ManageUsersRole'
                  :> Post '[HTML] Text

type DeleteRole = "users" :> Capture "uid" (Key User)
                  :> "roles" :> Capture "rid" (Key Role)
                  :> RequireAuth ManageUsersRole'
                  :> Delete '[HTML] Text
