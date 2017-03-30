{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Controllers.Problem where

import Database.Esqueleto
import qualified Database.Persist as P

import Utils.Controller
import Utils.ExtensibleRecords
import Models

instance HasController (Int64 -> HandlerT IO [ExpandedContestProblem]) where
  resourceController contestId = fmap cmerge <$> runQuery q
    where q = select $ from $ \(cp `InnerJoin` prob) -> do
                on     $ cp ^. ContestProblemProblem ==. prob ^. ProblemId
                where_ $ cp ^. ContestProblemContest ==. val (toSqlKey contestId)
                orderBy [ asc (cp ^. ContestProblemShortcode) ]
                return (cp, prob)
          cmerge (cp, p) =
            ExpandedContestProblem $ Ext (Var :: Var "id") (entityKey cp)
                                   $ rDel (Var :: Var "contest")
                                   $ rSet (Var :: Var "problem") (entityVal p)
                                   $ explode (entityVal cp)

instance HasController (Int64 -> Int64 -> HandlerT IO Problem) where
  resourceController _ problemId = fmap entityVal <$> runQuery $
    selectOne $ from $ \(cp `InnerJoin` prob) -> do
      on     $ cp ^. ContestProblemProblem ==. prob ^. ProblemId
      where_ $ cp ^. ContestProblemId ==. val (toSqlKey problemId)
      return prob

instance HasController (Int64 -> Int64 -> HandlerT IO [ProblemExampleWithoutProblem]) where
  resourceController _ problemId = fmap trans <$> runQuery q
    where q = select $ from $ \(cp `InnerJoin` exa) -> do
                on     $ cp ^. ContestProblemProblem ==. exa ^. ProblemExampleProblem
                where_ $ cp ^. ContestProblemId ==. val (toSqlKey problemId)
                orderBy [ asc (exa ^. ProblemExampleNumber) ]
                return exa

          trans a = ProblemExampleWithoutProblem $ rDel (Var :: Var "problem")
                                                 $ rDel (Var :: Var "number")
                                                 $ explode (entityVal a)

-- TODO
instance HasController (Int64 -> Int64 -> HandlerT IO [QuestionWithAnswers]) where
  resourceController = undefined
  -- resourceController _ problemId = fmap trans <$> runQuery q
  --   where q = select $ from $ \(que `LeftJoin` ans) -> do
  --               on     $ ans ^. AnswerQuestion ==. que ^. QuestionId
  --               where_ $ que ^. QuestionContest ==. val (toSqlKey contestId)
  --               orderBy [ asc (cp ^. ContestProblemShortcode) ]
  --               return (cp, prob)
  --         trans a = QuestionWithAnswer $ rDel (Var :: Var "problem")
  --                                      $ explode (entityVal a)

