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
import Database.Esqueleto
import qualified Data.Hashable as HM
import qualified Data.HashMap.Strict as HM
import qualified Database.Persist as P
import qualified Database.Persist.Postgresql as P
import Data.Monoid ((<>))
import Data.Int (Int64(..))
import qualified Data.Text as T

import Api
import Types
import Models

import Utils.ExtensibleRecords

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

getAll :: (PersistEntityBackend a ~ SqlBackend, PersistEntity a)
       => HandlerT IO [Entity a]
getAll = runQuery $ P.selectList [] []

getById id = do
  q <- runQuery $ get (toSqlKey id)
  maybe (throwError ErrNotFound) pure q

collectionJoin :: (HM.Hashable a, Eq a) => [(a, b)] -> [(a, [b])]
collectionJoin xs = HM.toList $ HM.fromListWith (++) [(k, [v]) | (k, v) <- xs]

class HasController a where
  resourceController :: a

instance (HasController a, HasController b) => HasController (a :<|> b) where
  resourceController = resourceController :<|> resourceController

instance ( PersistEntityBackend a ~ SqlBackend
         , PersistEntity a
         ) => HasController (HandlerT IO [Entity a]) where
  resourceController = getAll

instance HasController (HandlerT IO CurrentTime) where
  resourceController = liftIO getCurrentTime

instance HasController (Int64 -> HandlerT IO User) where
  resourceController = getById
instance HasController (Int64 -> HandlerT IO Contest) where
  resourceController = getById

instance HasController (Int64 -> HandlerT IO [ContestProblem]) where
  resourceController contestId = runQuery $ fmap entityVal <$>
    P.selectList [ContestProblemContest P.==. toSqlKey contestId] []

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


instance HasController (HandlerT IO [ContestWithOwners]) where
  resourceController = do
    let q = select $ from $ \(contests `InnerJoin` ownerships `InnerJoin` users) -> do
              on      $ users ^. UserId ==. ownerships ^. ContestOwnershipOwner
              on      $ ownerships ^. ContestOwnershipContest ==. contests ^. ContestId
              orderBy [ asc (contests ^. ContestName) ]
              return (contests, users)

        enrich :: (Entity Contest, [Entity User]) -> ContestWithOwners
        enrich (ec, eu) = ContestWithOwners $ (fmap entityVal eu)
                                           :& entityKey ec
                                           :& explode (entityVal ec)

    (fmap enrich . collectionJoin) <$> (runQuery q)


controller :: ServerT API (HandlerT IO)
controller = resourceController
