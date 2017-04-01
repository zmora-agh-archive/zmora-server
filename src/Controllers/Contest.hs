{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds         #-}

module Controllers.Contest where

import Database.Esqueleto

import Utils.Controller
import Utils.ExtensibleRecords
import Models

instance HasController (CurrentUser -> Int64 -> HandlerT IO ContestWithOwners) where
  resourceController _ contestId = do
    res <- runQuery $ select $ from $ \(contest `InnerJoin` ownerships `InnerJoin` users) -> do
            on      $ users ^. UserId ==. ownerships ^. ContestOwnershipOwner
            on      $ ownerships ^. ContestOwnershipContest ==. contest ^. ContestId
            where_  $ contest ^. ContestId ==. val (toSqlKey contestId)
            return (contest, users)

    (ec, eu) <- safeHead $ collectionJoin res
    return $ ContestWithOwners $ rAdd (Var :: Var "owners") (fmap entityVal eu)
                               $ explode (entityVal ec)

instance HasController (CurrentUser -> HandlerT IO [Entity' Contest ContestWithOwners]) where
  resourceController _ = (fmap enrich . collectionJoin) <$> runQuery q
    where q = select $ from $ \(contests `InnerJoin` ownerships `InnerJoin` users) -> do
                on      $ users ^. UserId ==. ownerships ^. ContestOwnershipOwner
                on      $ ownerships ^. ContestOwnershipContest ==. contests ^. ContestId
                orderBy [ asc (contests ^. ContestName) ]
                return (contests, users)

          enrich (ec, eu) = Entity' (entityKey ec)
                              $ ContestWithOwners
                              $ rAdd (Var :: Var "owners") (fmap entityVal eu)
                              $ explode (entityVal ec)

