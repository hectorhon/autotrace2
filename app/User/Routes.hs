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
             :<|> ToEditUser
             :<|> EditUser
             :<|> AssignRole
             :<|> DeleteRole
             :<|> DeleteUser

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

type ToEditUser = "users" :> Capture "uid" (Key User) :> "edit"
                  :> RequireAuth ManageUsersRole'
                  :> Get '[HTML] Html

type EditUser = "users" :> Capture "uid" (Key User) :> "edit"
                :> ReqBody '[FormUrlEncoded] LoginData
                :> RequireAuth ManageUsersRole'
                :> Post '[HTML] Text

type AssignRole = "users" :> Capture "uid" (Key User) :> "roles"
                  :> ReqBody '[FormUrlEncoded] Role
                  :> RequireAuth ManageUsersRole'
                  :> Post '[HTML] Text

type DeleteRole = "users" :> Capture "uid" (Key User) :> "roles"
                  :> RequireAuth ManageUsersRole'
                  :> Delete '[HTML] Text

type DeleteUser = "users" :> Capture "uid" (Key User) :> "edit"
                  :> RequireAuth ManageUsersRole'
                  :> Delete '[HTML] Text
