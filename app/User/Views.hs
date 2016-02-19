{-# LANGUAGE OverloadedStrings #-}

module User.Views where

import Text.Blaze.Html5 as H hiding (i)
import Text.Blaze.Html5.Attributes as Ha
import Database.Persist.Postgresql
import Control.Monad (when, forM_)
import Common.Views
import Home.Links
import User.Links
import User.Types

loginPage :: Bool -> Html
loginPage validLogin = layoutNoBanner "Login" $ do
  h1 "Login"
  H.form ! method "post" $ do
    H.label $ do
      H.span "Username"
      input ! Ha.name "name" ! Ha.value ""
    H.label $ do
      H.span "Password"
      input ! Ha.name "password" ! type_ "password" ! Ha.value ""
    H.div ! Ha.style "float:right;" $ do
      when (not validLogin) (H.span $ "Invalid username or password!")
      button "Login"
      a ! Ha.id "login-as-guest-link" ! href "#" $ "Login as guest"
  script $ toHtml $
    " $('#login-as-guest-link').click(function(e) {            \
    \   e.preventDefault();                                  \
    \   $.post('" ++ logoutLink' ++ "').success(function() { \
    \     window.location.href = '" ++ homePageLink' ++ "';   \
    \   });                                                  \
    \ });                                                    "

usersPage :: [Entity User] -> Html
usersPage users = layout "Users" $ do
  breadCrumbs [ ("Settings", Nothing)
              , ("Users", Nothing)
              ]
  h1 "Users"
  ul $ do
    forM_ users $ \ (Entity uid user) ->
      li $ a ! href (viewUserLink uid) $ toHtml (userName user)
    li $ a ! href toCreateUserLink $ "Create new user..."

userNewPage :: Html
userNewPage = layout "New user" $ do
  breadCrumbs [ ("Settings", Nothing)
              , ("Users", Just viewUsersLink)
              , ("New user", Nothing)
              ]
  h1 "New user"
  H.form ! Ha.name "new-user-form" ! method "post"
         ! onsubmit "if ($('#password').val()==$('#confirm-password').val()) {\
                    \  return true;                                           \
                    \} else {                                                 \
                    \  alert('Passwords do not match!');                      \
                    \  return false;                                          \
                    \}" $ do
    H.label $ H.span "Username" >> input ! Ha.name "name" ! Ha.value ""
    H.label $ do
      H.span "Password"
      input ! Ha.id "password" ! Ha.name "password" ! type_ "password"
            ! Ha.value ""
    H.label $ do
      H.span "Confirm password"
      input ! Ha.id "confirm-password" ! type_ "password" ! Ha.value ""
    button "Save"

userPage :: Entity User -> [Entity Role] -> Html
userPage (Entity uid user) roles = layout "View user details" $ do
  breadCrumbs [ ("Settings", Nothing)
              , ("Users", Just viewUsersLink)
              , ("View user (" ++ (userName user) ++ ")", Nothing)
              ]
  h1 "View user"
  h2 "Details"
  table ! class_ "definition-table" $ do
    tr $ do
      th "Username"
      td $ toHtml $ userName user
    tr $ do
      th "Password"
      td $ a ! href (toResetPasswordLink uid) $ "Reset password..."
    tr $ do
      th "Delete user"
      td $ a ! Ha.id "delete-user-link" ! href "#" $ "Delete user"
  script $ toHtml $
    " $('#delete-user-link').click(function(e) {                 \
    \   e.preventDefault();                                      \
    \   if (confirm('Delete this user?')) {                      \
    \     $.ajax({                                               \
    \       method: 'DELETE',                                    \
    \       url: window.location,                                \
    \       success: function() {                                \
    \         window.location.replace('"++ viewUsersLink' ++"'); \
    \       }                                                    \
    \     })                                                     \
    \   }                                                        \
    \ });                                                        "
  h2 "Roles"
  ul $ forM_ (zip ([1..]::[Int]) roles) $ \ (i, (Entity rid role)) -> do
    li $ do
      H.span $ toHtml (show (roleRole role))
      H.span " "
      let linkId = "delete-role-link-" ++ (show i)
      a ! Ha.id (stringValue linkId) ! href "#" $ "[X]"
      let redirectTo = viewUserLink' uid in script $ toHtml $
        " $('#"++ linkId ++"').click(function(e) {                \
        \   e.preventDefault();                                   \
        \   if (confirm('Delete this role?')) {                   \
        \     $.ajax({                                            \
        \       method: 'DELETE',                                 \
        \       url: '"++ (deleteRoleLink' uid rid) ++"',         \
        \       success: function() {                             \
        \         window.location.replace('"++ redirectTo ++"');  \
        \       }                                                 \
        \     })                                                  \
        \   }                                                     \
        \ });                                                     "
  p $ a ! href (toAssignRoleLink uid) $ "Assign new role..."

resetPasswordPage :: Entity User -> Html
resetPasswordPage (Entity uid user) = layout "Reset password" $ do
  breadCrumbs [ ("Settings", Nothing)
              , ("Users", Just viewUsersLink)
              , ("View user ("++(userName user)++")", Just (viewUserLink uid))
              , ("Reset password", Nothing)
              ]
  h1 "View user"
  h2 "Reset password"
  H.form ! Ha.name "reset-password-form" ! method "post"
         ! onsubmit "if ($('#password').val()==$('#confirm-password').val()) {\
                    \  return true;                                           \
                    \} else {                                                 \
                    \  alert('Passwords do not match!');                      \
                    \  return false;                                          \
                    \}" $ do
    hiddenField "name" (userName user)
    linkField "Username" (userName user) (viewUserLink uid)
    H.label $ do
      H.span "New password"
      input ! Ha.id "password" ! Ha.name "password" ! type_ "password"
            ! Ha.value ""
    H.label $ do
      H.span "Confirm password"
      input ! Ha.id "confirm-password" ! type_ "password" ! Ha.value ""
    button "Save"
    cancelButton "reset-password-cancel-button"

assignRolePage :: Entity User -> Html
assignRolePage (Entity uid user) = layout "Assign new role" $ do
  breadCrumbs [ ("Settings", Nothing)
              , ("Users", Just viewUsersLink)
              , ("View user ("++(userName user)++")", Just (viewUserLink uid))
              , ("Assign new role", Nothing)
              ]
  h1 "View user"
  h2 "Assign new role"
  H.form ! method "post" $ do
    hiddenField "uid" (show $ fromSqlKey uid)
    linkField "Username" (userName user) (viewUserLink uid)
    selectField "Role" "role" undefined Nothing
      [ (AreaAdminRole, "Area admin")
      , (ControlAdminRole, "Control admin")
      , (LopcAdminRole, "LOPC admin")
      , (LopcUserRole, "LOPC user")
      , (ManageUsersRole, "Manage users")
      ]
    button "Save"
    cancelButton "assign-role-cancel-button"
