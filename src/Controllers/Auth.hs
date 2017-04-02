{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Controllers.Auth where

import Control.Monad.Except
import Crypto.Scrypt
import Data.ByteString.Lazy
import Data.Monoid ((<>))
import Data.Text as T
import Data.Text.Encoding
import Database.Esqueleto
import Diener
import Servant
import Servant.Auth.Server

import Utils.Controller
import Models

instance FromJWT CurrentUser
instance ToJWT CurrentUser

instance ( Monad m
         , HasController (CurrentUser -> b)
         , HasController (CurrentUser -> HandlerT m a)
         ) => HasController (AuthResult CurrentUser -> HandlerT m a :<|> b) where
  resourceController = \case
    Authenticated user -> resourceController user :<|> resourceController user
    _                  -> throwError ErrUnauthorized :<|> error "Impossible happened"

instance ( HasController (CurrentUser -> a)
         , HasController (CurrentUser -> b)
         ) => HasController (CurrentUser -> a :<|> b) where
  resourceController x = resourceController x :<|> resourceController x

instance HasController (Login -> HandlerT IO JwtToken) where
  resourceController a@(Login nick candidatePass) = do
    let q = selectOne $ from $ \(cred `InnerJoin` user) -> do
              on      $ user ^. UserId ==. cred ^. CredentialUser
              where_  $ user ^. UserNick ==. val nick
              return (cred ^. CredentialHash, cred ^. CredentialSalt, user)

    (Value dbHash, Value passSalt, user) <- runQuery q

    let candidateHash = scrypt' (Salt passSalt) (Pass $ encodeUtf8 candidatePass)

    if getHash candidateHash == dbHash
      then do
        jwtCfg <- asks jwtSettings
        token <- liftIO $ makeJWT user jwtCfg Nothing
        case token of
          Left e -> do
            $logError $ "JWT Error: " <> (T.pack . show) e
            throwError ErrUnauthorized
          Right r -> return $ JwtToken $ decodeUtf8 $ toStrict r
      else throwError ErrUnauthorized
