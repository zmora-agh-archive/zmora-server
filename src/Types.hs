module Types where

import Diener (DienerT(..))
import Database.Persist.Postgresql (ConnectionPool)

data AppError = ErrNotFound | ErrDatabaseQuery deriving Show
type HandlerT = DienerT AppError HandlerEnv
data HandlerEnv = HandlerEnv { db :: ConnectionPool }


