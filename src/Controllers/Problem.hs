{-# LANGUAGE FlexibleInstances #-}
module Controllers.Problem where

import Database.Esqueleto

import Utils.Controller
import Models

instance HasController (Int64 -> HandlerT IO [ContestProblem]) where
  resourceController contestId = fmap entityVal <$> runQuery q
    where q = select $ from $ \cp -> do
                where_ $ cp ^. ContestProblemContest ==. val (toSqlKey contestId)
                return cp

instance HasController (Int64 -> Int64 -> HandlerT IO ContestProblem) where
  resourceController contestId problemId = undefined -- TODO

instance HasController (Int64 -> Int64 -> HandlerT IO [Entity ProblemExample]) where
  resourceController contestId problemId = undefined -- TODO

instance HasController (Int64 -> Int64 -> HandlerT IO [Entity Question]) where
  resourceController contestId problemId = undefined
