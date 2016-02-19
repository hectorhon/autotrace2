{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module User.Links where

import Servant
import Database.Persist.Postgresql
import Text.Blaze.Html5
import Common.Links
import User.Routes
import User.Types

toLoginLink' :: Bool -> String
toLoginLink' isValid = "/" ++ show (linkTo (Proxy :: Proxy ToLogin) isValid)

logoutLink' :: String
logoutLink' = "/" ++ show (linkTo (Proxy :: Proxy Logout))



viewUsersLink' :: String
viewUsersLink' = "/" ++ show (linkTo (Proxy :: Proxy ViewUsers))

viewUsersLink :: AttributeValue
viewUsersLink = stringValue (viewUsersLink')



toCreateUserLink :: AttributeValue
toCreateUserLink = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ToCreateUser))



viewUserLink' :: Key User -> String
viewUserLink' uid = "/" ++ show (linkTo (Proxy :: Proxy ViewUser) uid)

viewUserLink :: Key User -> AttributeValue
viewUserLink uid = stringValue (viewUserLink' uid)



toResetPasswordLink :: Key User -> AttributeValue
toResetPasswordLink uid = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ToResetPassword) uid)

deleteRoleLink' :: Key User -> Key Role -> String
deleteRoleLink' uid rid =
  "/" ++ show (linkTo (Proxy :: Proxy DeleteRole) uid rid)

toAssignRoleLink :: Key User -> AttributeValue
toAssignRoleLink uid = stringValue $
  "/" ++ show (linkTo (Proxy :: Proxy ToAssignRole) uid)
