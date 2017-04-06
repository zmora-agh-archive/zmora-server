{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}

module Controllers.Permission where

import Database.Esqueleto

import Types
import Models

import Utils.Controller

instance HasPermission CurrentUser Contest where
  authorize currentUser contestId =
    runQuery $ selectOne' ErrUnauthorized $ from $ \particip -> do
      where_ $ particip ^. ContestParticipationContest ==. val contestId
      where_ $ particip ^. ContestParticipationUser ==. val (entityKey currentUser)
