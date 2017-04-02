{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}

module Controllers.Submit where

import Database.Esqueleto

import Utils.Controller
import Utils.ExtensibleRecords

import Models

instance HasController (CurrentUser -> Int64 -> Int64 -> HandlerT IO [Entity Submit]) where
  resourceController user _ problemId = runQuery q
    where q = select $ from $ \submits -> do
            where_ $ submits ^. SubmitProblem ==. val (toSqlKey problemId)
            where_ $ submits ^. SubmitAuthor ==. val (entityKey user)
            return submits

instance HasController (CurrentUser -> Int64 -> Int64 -> Int64 -> HandlerT IO (Entity Submit)) where
  resourceController user _ _ submitId = runQuery q
    where q = selectOne $ from $ \submits -> do
            where_ $ submits ^. SubmitId ==. val (toSqlKey submitId)
            where_ $ submits ^. SubmitAuthor ==. val (entityKey user)
            return submits
