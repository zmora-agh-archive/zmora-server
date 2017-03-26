{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Controller where

import Servant
import Diener
import Control.Exception.Lifted (SomeException (..), catch)
import Database.Persist
import Database.Persist.Postgresql
import Data.Monoid ((<>))
import Data.Int (Int64(..))
import qualified Data.Text as T

import Api
import Types
import Models

runDb :: (MonadLogger (HandlerT IO), MonadError e (HandlerT IO))
      => ConnectionPool -> e -> SqlPersistT (HandlerT IO) a -> HandlerT IO a
runDb pool err q =
  catch (runSqlPool q pool) $ \(SomeException e) -> do
    $logError "runSqlPool failed."
    $logError $ "Error: " <> (T.pack . show) e
    throwError err

runQuery :: SqlPersistT (HandlerT IO) a -> HandlerT IO a
runQuery query = do
  pool <- asks db
  runDb pool ErrDatabaseQuery query

resAll :: (PersistEntityBackend record ~ SqlBackend, PersistEntity record)
       => HandlerT IO [Entity record]
resAll = runQuery $ selectList [] []

resById :: ToBackendKey SqlBackend record => Int64 -> HandlerT IO record
resById id = do
  q <- runQuery $ get (toSqlKey id)
  maybe (throwError ErrNotFound) pure q

controller :: ServerT API (HandlerT IO)
controller = undefined -- FIXME
