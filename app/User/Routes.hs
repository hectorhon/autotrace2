{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module User.Routes where

import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5
import Data.Text
import User.Types

type UserRoutes = ToLogin
             :<|> Login
             :<|> Logout

type ToLogin = "login" :> QueryParam "valid" Bool :> Get '[HTML] Html

type Login = "login"
             :> ReqBody '[FormUrlEncoded] LoginData
             :> Post '[PlainText] Text

type Logout = "logout" :> Header "Cookie" String :> Post '[PlainText] Text
