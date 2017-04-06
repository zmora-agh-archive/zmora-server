{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds         #-}

module Controllers.Contest where

import Controllers.Permission

import Database.Esqueleto
import Data.Maybe (isJust)

import Utils.Controller
import Utils.ExtensibleRecords
import Models

instance HasController (CurrentUser -> Key Contest -> HandlerT IO (Entity' Contest ContestWithOwners)) where
  resourceController currentUser contestId = do
    authorize currentUser contestId -- TODO: Do it in safer style

    res <- runQuery $ select $ from $ \(contest `InnerJoin` ownerships `InnerJoin` users) -> do
            on      $ users ^. UserId ==. ownerships ^. ContestOwnershipOwner
            on      $ ownerships ^. ContestOwnershipContest ==. contest ^. ContestId
            where_  $ contest ^. ContestId ==. val contestId
            return (contest, users)

    (ec, eu) <- safeHead ErrNotFound $ collectionJoin res
    return $ Entity' (entityKey ec)
              $ ContestWithOwners
              $ rAdd (Var :: Var "joined") True -- Otherwise user should get unauthorized
              $ rAdd (Var :: Var "owners") (fmap entityVal eu)
              $ explode (entityVal ec)

instance HasController (CurrentUser -> HandlerT IO [Entity' Contest ContestWithOwners]) where
  resourceController currentUser = (fmap enrich . collectionJoin) <$> runQuery q
    where q = select $ from $ \(contests `InnerJoin` ownerships `InnerJoin` users `LeftOuterJoin` particip) -> do
                on      $     (particip ?. ContestParticipationContest ==. just (contests ^. ContestId))
                          &&. (particip ?. ContestParticipationUser ==. just (val $ entityKey currentUser))
                on      $ users ^. UserId ==. ownerships ^. ContestOwnershipOwner
                on      $ ownerships ^. ContestOwnershipContest ==. contests ^. ContestId
                orderBy [ asc (contests ^. ContestName) ]
                return ((contests, particip), users)

          enrich ((ec, pt), eu) = Entity' (entityKey ec)
                              $ ContestWithOwners
                              $ rAdd (Var :: Var "joined") (isJust pt)
                              $ rAdd (Var :: Var "owners") (fmap entityVal eu)
                              $ explode (entityVal ec)

