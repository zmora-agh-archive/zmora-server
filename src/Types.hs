{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Servant
import Servant.Auth.Server (JWTSettings, CookieSettings)

import Diener (DienerT(..))
import Database.Persist.Postgresql (ConnectionPool)

data AppError = ErrNotFound
              | ErrDatabaseQuery
              | ErrUnauthorized
              | ErrForbidden
              deriving Show

type HandlerT = DienerT AppError HandlerEnv

data HandlerEnv = HandlerEnv { db   :: ConnectionPool
                             , jwtSettings :: JWTSettings
                             }


