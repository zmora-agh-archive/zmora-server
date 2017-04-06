{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}

module Controllers.Submit where

import Database.Esqueleto

import Utils.Controller
import Utils.ExtensibleRecords

import Models

instance HasController (CurrentUser -> Key Contest -> Key ContestProblem -> HandlerT IO [Entity Submit]) where
  resourceController user _ problemId = runQuery q
    where q = select $ from $ \submits -> do
            where_ $ submits ^. SubmitProblem ==. val problemId
            where_ $ submits ^. SubmitAuthor ==. val (entityKey user)
            return submits

instance HasController (CurrentUser -> Key Contest -> Key ContestProblem -> Key Submit -> HandlerT IO (Entity Submit)) where
  resourceController user _ _ submitId = runQuery q
    where q = selectOne $ from $ \submits -> do
            where_ $ submits ^. SubmitId ==. val submitId
            where_ $ submits ^. SubmitAuthor ==. val (entityKey user)
            return submits
