{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings     #-}

module Utils.Controller
  ( module Utils.Controller
  , module Types
  , Int64(..)
  , Entity(..)
  ) where

import Servant
import Diener
import Data.Int
import Data.Monoid ((<>))
import Control.Exception.Lifted (SomeException (..), catch)
import Database.Persist
import Database.Persist.Postgresql
import qualified Database.Esqueleto as E
import qualified Data.Hashable as HM
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T (pack)

import Types

class HasController a where
  resourceController :: a

instance (HasController a, HasController b) => HasController (a :<|> b) where
  resourceController = resourceController :<|> resourceController

instance ( PersistEntityBackend a ~ SqlBackend
         , PersistEntity a
         ) => HasController (HandlerT IO [Entity a]) where
  resourceController = getAll

runDb :: (MonadLogger (HandlerT IO), MonadError e (HandlerT IO))
      => ConnectionPool -> e -> SqlPersistT (HandlerT IO) a -> HandlerT IO a
runDb pool err q =
  catch (runSqlPool q pool) $ \(SomeException e) -> do
    $logError "runSqlPool failed."
    $logError $ "Error: " <> (T.pack . show) e
    throwError err

runQuery query = do
  pool <- asks db
  runDb pool ErrDatabaseQuery query

selectOne a = do
  a <- E.select a
  let safeHead []     = Nothing
      safeHead (a:as) = Just a
  maybe (throwError ErrNotFound) pure (safeHead a)

getAll :: (PersistEntityBackend a ~ SqlBackend, PersistEntity a)
       => HandlerT IO [Entity a]
getAll = runQuery $ selectList [] []

getById id = do
  q <- runQuery $ get (toSqlKey id)
  maybe (throwError ErrNotFound) pure q

collectionJoin :: (HM.Hashable a, Eq a) => [(a, b)] -> [(a, [b])]
collectionJoin xs = HM.toList $ HM.fromListWith (++) [(k, [v]) | (k, v) <- xs]
