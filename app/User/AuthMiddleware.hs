{-# LANGUAGE OverloadedStrings #-}

module User.AuthMiddleware where

import Network.Wai
import Database.Esqueleto
import Control.Exception
import Data.ByteString.Char8 (pack)
import User.Types

auth :: ConnectionPool -> Middleware
auth connPool app rq k =
  let filtered = filter ((/= "UserRoles") . fst) (requestHeaders rq) in
  do case lookup "Cookie" (requestHeaders rq) of
       Nothing -> app rq k
       Just tk -> do
         vu <- (flip runSqlPool) connPool $
           select $ from $ \ (r `InnerJoin` u `InnerJoin` s) -> do
             on (s ^. SessionUser ==. u ^. UserId)
             on (r ^. RoleUser ==. u ^. UserId)
             where_ (s ^. SessionIdent ==. val tk)
             return (u ^. UserName, r ^. RoleRole)
         if null vu then app rq k
         else do let u = map (\ (a, b) -> (unValue a, unValue b)) vu
                 assert (and $ map (\ (a,_) -> a == fst (head u)) u) (return ())
                 let u' = (fst (head u), map snd u)
                 let p = ("UserRoles", pack $ show u')
                 let rq' = rq { requestHeaders = p : filtered }
                 app rq' k
