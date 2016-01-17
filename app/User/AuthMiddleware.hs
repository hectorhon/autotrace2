{-# LANGUAGE OverloadedStrings #-}

module User.AuthMiddleware where

import Network.Wai
import Database.Esqueleto
import Data.ByteString.Char8 (pack, unpack)
import User.Types
import Web.Cookie

auth :: ConnectionPool -> Middleware
auth connPool app rq k =
  let filtered = filter ((/= "Roles") . fst) (requestHeaders rq)
      mUI = do cookies  <- fmap parseCookies $
                           lookup "Cookie" (requestHeaders rq)
               username <- fmap unpack (lookup "username" cookies)
               ident    <- lookup "ident" cookies
               return (username, ident)
  in case mUI of
       Nothing -> app rq k
       Just (username, ident) -> do
         vr <- (flip runSqlPool) connPool $
           select $ from $ \ (r `InnerJoin` u `InnerJoin` s) -> do
             on (s ^. SessionUser ==. u ^. UserId)
             on (r ^. RoleUser ==. u ^. UserId)
             where_ ( s ^. SessionIdent ==. val ident
                      &&. u ^. UserName ==. val username)
             return (r ^. RoleRole)
         if null vr then app rq k
         else let p = ("Roles", pack $ show $ map unValue vr)
                  rq' = rq { requestHeaders = p : filtered }
              in app rq' k
