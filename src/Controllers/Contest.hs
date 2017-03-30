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
  resourceController = do
    let q = select $ from $ \(contests `InnerJoin` ownerships `InnerJoin` users) -> do
              on      $ users ^. UserId ==. ownerships ^. ContestOwnershipOwner
              on      $ ownerships ^. ContestOwnershipContest ==. contests ^. ContestId
              orderBy [ asc (contests ^. ContestName) ]
              return (contests, users)

        enrich :: (Entity Contest, [Entity User]) -> ContestWithOwners
        enrich (ec, eu) = ContestWithOwners $ Ext (Var :: Var "owners") (fmap entityVal eu)
                                            $ Ext (Var :: Var "id") (entityKey ec)
                                            $ explode (entityVal ec)
    (fmap enrich . collectionJoin) <$> runQuery q
