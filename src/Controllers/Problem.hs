{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Controllers.Problem where

import Database.Esqueleto
import qualified Database.Persist as P
import Data.Maybe (mapMaybe)
import Control.Lens (_1)

import Utils.Controller
import Utils.ExtensibleRecords
import Models

instance HasController (CurrentUser -> Key Contest -> HandlerT IO [Entity' ContestProblem ExpandedContestProblem]) where
  resourceController _ contestId = fmap trans <$> runQuery q
    where q = select $ from $ \(cp `InnerJoin` prob) -> do
                on     $ cp ^. ContestProblemProblem ==. prob ^. ProblemId
                where_ $ cp ^. ContestProblemContest ==. val contestId
                orderBy [ asc (cp ^. ContestProblemShortcode) ]
                return (cp, prob)

          trans (cp, p) = Entity' (entityKey cp)
                            $ ExpandedContestProblem
                            $ rSet (Var :: Var "problem") (entityVal p)
                            $ explode (entityVal cp)

instance HasController (CurrentUser -> Key Contest -> Key ContestProblem -> HandlerT IO (Entity' ContestProblem ExpandedContestProblem)) where
  resourceController _ _ problemId = trans <$> runQuery q
    where q = selectOne $ from $ \(cp `InnerJoin` prob) -> do
                on     $ cp ^. ContestProblemProblem ==. prob ^. ProblemId
                where_ $ cp ^. ContestProblemId ==. val problemId
                return (cp, prob)

          trans (cp, p) = Entity' (entityKey cp)
                            $ ExpandedContestProblem
                            $ rSet (Var :: Var "problem") (entityVal p)
                            $ explode (entityVal cp)


instance HasController (CurrentUser -> Key Contest -> Key ContestProblem -> HandlerT IO [ProblemExample]) where
  resourceController _ _ problemId = fmap entityVal <$> runQuery q
    where q = select $ from $ \(cp `InnerJoin` exa) -> do
                on     $ cp ^. ContestProblemProblem ==. exa ^. ProblemExampleProblem
                where_ $ cp ^. ContestProblemId ==. val problemId
                orderBy [ asc (exa ^. ProblemExampleNumber) ]
                return exa

-- TODO Check if join types are correct
instance HasController (CurrentUser -> Key Contest -> Key ContestProblem -> HandlerT IO [QuestionWithAnswers]) where
  resourceController _ _ problemId = (fmap trans . collectionJoin) <$> runQuery q
    where q = select $ from $ \(cp `RightOuterJoin` que `LeftOuterJoin` ans `InnerJoin` aaut `InnerJoin` qaut) -> do
                on     $ que ^. QuestionAuthor ==. qaut ^. UserId
                on     $ ans ?. AnswerAuthor ==. just (aaut ^. UserId)
                on     $ ans ?. AnswerQuestion ==. just (que ^. QuestionId)
                on     $ cp  ^. ContestProblemId ==. que ^. QuestionProblem
                where_ $ cp ^. ContestProblemId ==. val problemId
                orderBy [ asc (que ^. QuestionAsked) ]
                return ((que, qaut), (ans, aaut))

          trans ((que, qaut), ans) = QuestionWithAnswers
            $ rAdd (Var :: Var "answers") (trans' <$> mapMaybe (_1 id) ans)
            $ rSet (Var :: Var "author") (entityVal qaut)
            $ explode (entityVal que)

          trans' (ans, aaut) = ExpandedAnswer $ rSet (Var :: Var "author") (entityVal aaut)
                                              $ explode (entityVal ans)
