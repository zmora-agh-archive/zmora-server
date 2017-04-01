{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}

module Controllers.Submit where

import Database.Esqueleto

import Utils.Controller
import Utils.ExtensibleRecords

import Models

instance HasController (CurrentUser -> Int64 -> Int64 -> HandlerT IO [Entity' Submit SubmitWithoutAuthor]) where
  resourceController user _ problemId = fmap trans <$> runQuery q
    where q = select $ from $ \submits -> do
            where_ $ submits ^. SubmitProblem ==. val (toSqlKey problemId)
            where_ $ submits ^. SubmitAuthor ==. val (entityKey user)
            return submits

          trans a = Entity' (entityKey a)
                      $ SubmitWithoutAuthor
                      $ rDel (Var :: Var "author")
                      $ explode (entityVal a)

instance HasController (CurrentUser -> Int64 -> Int64 -> Int64 -> HandlerT IO SubmitWithoutAuthor) where
  resourceController user _ _ submitId = trans <$> runQuery q
    where q = selectOne $ from $ \submits -> do
            where_ $ submits ^. SubmitId ==. val (toSqlKey submitId)
            where_ $ submits ^. SubmitAuthor ==. val (entityKey user)
            return submits

          trans a = SubmitWithoutAuthor $ rDel (Var :: Var "author")
                                        $ explode (entityVal a)
