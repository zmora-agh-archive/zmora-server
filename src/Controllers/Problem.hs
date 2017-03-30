{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Controllers.Problem where

import Database.Esqueleto
import qualified Database.Persist as P
import Data.Maybe (catMaybes)

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
            ExpandedContestProblem $ rAdd (Var :: Var "id") (entityKey cp)
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

-- TODO Check if join types are correct
instance HasController (Int64 -> Int64 -> HandlerT IO [QuestionWithAnswers]) where
  resourceController _ problemId = (fmap trans . collectionJoin) <$> runQuery q
    where q = select $ from $ \(cp `RightOuterJoin` que `LeftOuterJoin` ans `InnerJoin` aaut `InnerJoin` qaut) -> do
                on     $ que ^. QuestionAuthor ==. qaut ^. UserId
                on     $ ans ?. AnswerAuthor ==. just (aaut ^. UserId)
                on     $ ans ?. AnswerQuestion ==. just (que ^. QuestionId)
                on     $ cp  ^. ContestProblemId ==. que ^. QuestionProblem
                where_ $ cp ^. ContestProblemId ==. val (toSqlKey problemId)
                orderBy [ asc (que ^. QuestionAsked) ]
                return ((que, qaut), (ans, aaut))

          -- TODO Replace with oneliner
          transMaybes :: [(Maybe a, b)] -> [(a, b)]
          transMaybes ((Just a, b) : xs) = (a, b) : transMaybes xs
          transMaybes (_ : xs) = transMaybes xs
          transMaybes [] = []

          trans ((que, qaut), ans) = QuestionWithAnswers
            $ rAdd (Var :: Var "answers") (trans' <$> transMaybes ans)
            $ rDel (Var :: Var "problem")
            $ rSet (Var :: Var "author") (entityVal qaut)
            $ explode (entityVal que)

          trans' (ans, aaut) = AnswerWithoutQuestion $ rDel (Var :: Var "question")
                                                     $ rSet (Var :: Var "author") (entityVal aaut)
                                                     $ explode (entityVal ans)