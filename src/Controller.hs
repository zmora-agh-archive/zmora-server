{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Controller where

import Servant
import Diener
import Control.Monad.IO.Class (liftIO)
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

getById id = do
  q <- runQuery $ get (toSqlKey id)
  maybe (throwError ErrNotFound) pure q

class HasController a where
  resourceController :: a

instance (HasController a, HasController b) => HasController (a :<|> b) where
  resourceController = resourceController :<|> resourceController

instance ( PersistEntityBackend a ~ SqlBackend
         , PersistEntity a
         ) => HasController (HandlerT IO [Entity a]) where
  resourceController = runQuery $ selectList [] []

instance HasController (HandlerT IO CurrentTime) where
  resourceController = liftIO $ getCurrentTime

instance HasController (Int64 -> HandlerT IO User) where
  resourceController = getById
instance HasController (Int64 -> HandlerT IO Contest) where
  resourceController = getById

instance HasController (Int64 -> HandlerT IO [Entity ContestProblem]) where
  resourceController contestId = undefined -- TODO
instance HasController (Int64 -> Int64 -> HandlerT IO ContestProblem) where
  resourceController contestId problemId = undefined -- TODO

instance HasController (Int64 -> Int64 -> HandlerT IO [Entity ProblemExample]) where
  resourceController contestId problemId = undefined -- TODO
instance HasController (Int64 -> Int64 -> HandlerT IO [Entity Question]) where
  resourceController contestId problemId = undefined -- TODO

instance HasController (Int64 -> Int64 -> HandlerT IO [Entity Submit]) where
  resourceController contestId problemId = undefined -- TODO
instance HasController (Int64 -> Int64 -> Int64 -> HandlerT IO Submit) where
  resourceController contestId problemId submitId = undefined -- TODO

controller :: ServerT API (HandlerT IO)
controller = resourceController
