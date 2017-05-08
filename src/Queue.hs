{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Queue where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import qualified Network.AMQP           as AMQP

import           Database.Esqueleto
import qualified Models                 as DM
import qualified Models.SubmitStatus    as S
import           Utils.Controller       (HandlerT, taskPubConnection)

import           Diener

import           Zmora.AMQP
import           Zmora.Queue

connectQueue :: String -> IO AMQP.Connection
connectQueue = AMQP.openConnection'' . AMQP.fromURI

--
-- Queue: task submission (server → judge)
--
submitTask
  :: ConnectionPool
  -> Entity DM.Submit
  -> [(T.Text, BS.ByteString)]
  -> HandlerT IO (Maybe AMQP.ConfirmationResult)
submitTask dbPool submitEntity filesContents = do
  connection <- asks taskPubConnection
  publishResult <-
    do taskTests <- liftIO $ problemTests dbPool problemId
       let task = Task taskEntityId config taskFiles taskTests
       $logDebug $ "Publishing task " <> (T.pack . show) task
       liftIO $
         withTaskPublisher connection $ \publisher -> publish publisher task
  $logDebug $ "Publish result: " <> (T.pack . show) publishResult
  return publishResult
  where
    taskEntityId = fromSqlKey . entityKey $ submitEntity
    (DM.Submit problemId _ _ _) = entityVal submitEntity
    config = "dummy config"
    taskFiles =
      map
        (\(filename, contents) -> File filename $ BL.fromStrict contents)
        filesContents

--
-- Queue: handling task results
--
runDbQueueSubscriber :: ConnectionPool -> AMQP.Connection -> IO AMQP.ConsumerTag
runDbQueueSubscriber dbPool qConn = do
  subscriber <- taskResultSubscriber qConn
  subscribe subscriber $ handleTaskResultMsg dbPool

handleTaskResultMsg :: ConnectionPool -> (TaskResult, AMQP.Envelope) -> IO ()
handleTaskResultMsg dbPool (taskResult, env) = do
  -- FIXME run all queries in a single transaction
  mapM_ (`runSqlPool` dbPool) (insert <$> toDbModel taskResult)
  updateSubmitStatus dbPool taskResult
  liftIO $ AMQP.ackEnv env

--
-- Queue <-> database model mapping
--
toDbModel :: TaskResult -> [DM.TestResult]
toDbModel (TaskResult taskId _ results) =
  map
    (\(TestResult testId code exTime ramUsage) ->
       let dbId = toSqlKey taskId
           status = T.pack . show $ code
       in DM.TestResult dbId (toSqlKey testId) status exTime ramUsage)
    results

toQueueTest :: Entity DM.ProblemTest -> Test
toQueueTest test = Test (fromSqlKey . entityKey $ test) testInput testOutput 0 0
  where
    (DM.ProblemTest _ testInput testOutput) = entityVal test

--
-- Database data manipulation
--
updateSubmitStatus :: ConnectionPool -> TaskResult -> IO ()
updateSubmitStatus dbPool result =
  flip runSqlPool dbPool $ update $ \p -> do
      set p [DM.SubmitStatus =. val (submitStatus . testResults $ result)]
      where_ $ p ^. DM.SubmitId ==. val (toSqlKey . resultId $ result)

problemTests :: ConnectionPool -> Key DM.ContestProblem -> IO [Test]
problemTests dbPool problemId = map toQueueTest <$> runSqlPool q dbPool
  where
    q =
      select $
      from $ \testEntities -> do
        where_ $ testEntities ^. DM.ProblemTestProblem ==. val problemId
        return testEntities

--
-- Utilities
--
submitStatus :: [TestResult] -> S.SubmitStatus
submitStatus results =
  case results of
    [] -> S.QUE
    _ ->
      if all (\z -> status z == OK) results
        then S.OK
        else S.ERR

