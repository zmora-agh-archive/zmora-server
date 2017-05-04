{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Queue where

import           Control.Monad.IO.Class      (liftIO)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BL
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as T
import qualified Network.AMQP                as AMQP

import qualified Models                      as DM
import           Utils.Controller            (HandlerT, taskPubConnection)
import           Database.Esqueleto

import           Diener

import           Zmora.Queue
import           Zmora.AMQP

connectQueue :: String -> IO AMQP.Connection
connectQueue = AMQP.openConnection'' . AMQP.fromURI

toDbModel :: TaskResult -> [DM.TestResult]
toDbModel (TaskResult taskId _ results) =
  map
    (\(TestResult code exTime ramUsage) ->
       let dbId = toSqlKey taskId
           status = T.pack . show $ code
       in DM.TestResult dbId status exTime ramUsage)
    results

runDbQueueSubscriber :: ConnectionPool -> AMQP.Connection -> IO AMQP.ConsumerTag
runDbQueueSubscriber dbPool qConn = do
  subscriber <- taskResultSubscriber qConn
  subscribe subscriber $ \(msg, env) -> do
    mapM_ (`runSqlPool` dbPool) (insert <$> toDbModel msg)
    AMQP.ackEnv env

toQueueTest :: DM.ProblemTest -> Test
toQueueTest (DM.ProblemTest _ testInput testOutput) = Test testInput testOutput 0 0

problemTests :: ConnectionPool -> Key DM.ContestProblem -> IO [Test]
problemTests dbPool problemId =
  map (toQueueTest . entityVal) <$> runSqlPool q dbPool
  where
    q = select $ from $ \testEntities -> do
      where_ $ testEntities ^. DM.ProblemTestProblem ==. val problemId
      return testEntities

submitTask
  :: ConnectionPool
  -> Entity DM.Submit
  -> [(T.Text, BS.ByteString)]
  -> HandlerT IO (Maybe AMQP.ConfirmationResult)
submitTask dbPool submitEntity filesContents = do
  connection <- asks taskPubConnection

  publishResult <- do
    taskTests <- liftIO $ problemTests dbPool problemId
    let task = Task taskEntityId config taskFiles taskTests

    $logDebug $ "Publishing task " <> (T.pack . show) task
    liftIO $ withTaskPublisher connection $ \publisher -> publish publisher task

  $logDebug $ "Publish result: " <> (T.pack . show) publishResult

  return publishResult
  where
    taskEntityId = fromSqlKey . entityKey $ submitEntity
    (DM.Submit problemId _ _) = entityVal submitEntity
    config = "dummy config"
    taskFiles =
      map
        (\(filename, contents) -> File filename $ BL.fromStrict contents)
        filesContents
