{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds         #-}

module Controllers.Contest where

import Database.Esqueleto

import Utils.Controller
import Utils.ExtensibleRecords
import Models

instance HasController (Int64 -> HandlerT IO Contest) where
  resourceController = getById

instance HasController (HandlerT IO [ContestWithOwners]) where
  resourceController = (fmap enrich . collectionJoin) <$> runQuery q
    where q = select $ from $ \(contests `InnerJoin` ownerships `InnerJoin` users) -> do
              on      $ users ^. UserId ==. ownerships ^. ContestOwnershipOwner
              on      $ ownerships ^. ContestOwnershipContest ==. contests ^. ContestId
              orderBy [ asc (contests ^. ContestName) ]
              return (contests, users)

          enrich (ec, eu) = ContestWithOwners $ rAdd (Var :: Var "owners") (fmap entityVal eu)
                                              $ rAdd (Var :: Var "id") (entityKey ec)
                                              $ explode (entityVal ec)

