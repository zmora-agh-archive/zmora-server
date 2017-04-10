{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds         #-}

module Controllers.User where

import Control.Monad.IO.Class (liftIO)
import Crypto.Scrypt
import Database.Esqueleto
import Database.Persist (insert)
import Utils.Controller
import Utils.ExtensibleRecords
import Data.Text.Encoding

import Controllers.Auth (genToken)

import Models
import qualified Models.Group as G

instance HasController (UserRegistration -> HandlerT IO JwtToken) where
  resourceController (UserRegistration reg) = do
    let email    = rGet (Var :: Var "email") reg
        password = encodeUtf8 $ rGet (Var :: Var "password") reg

    (salt, hash) <- liftIO $ do
      s <- newSalt
      let h = scrypt' s (Pass password)
      return (getSalt s, getHash h)

    let user = implode reg :: User

    -- TODO Make sure that this happens in a single transaction
    uid <- runQuery $ do
      uid <- insert user
      insert $ Credential email salt hash uid G.UserGroup
      return uid

    genToken $ Entity uid user
