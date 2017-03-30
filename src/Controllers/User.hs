{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds         #-}

module Controllers.User where

import Control.Monad.IO.Class (liftIO)
import Crypto.Scrypt
import Database.Persist
import Utils.Controller
import Utils.ExtensibleRecords
import Data.Text.Encoding

import Models
import qualified Models.Group as G

instance HasController (Int64 -> HandlerT IO User) where
  resourceController = getById

instance HasController (UserRegistration -> HandlerT IO (Key User)) where
  resourceController (UserRegistration reg) = do
    let email    = rGet (Var :: Var "email") reg
        password = encodeUtf8 $ rGet (Var :: Var "password") reg

    (salt, hash) <- liftIO $ do
      s <- newSalt
      let h = scrypt' s (Pass password)
      return (getSalt s, getHash h)

    -- TODO Make sure that this happens in a single transaction
    id <- runQuery $ do
      uid <- insert (implode reg :: User)
      insert $ Credential email salt hash uid G.UserGroup
      return uid

    return id
