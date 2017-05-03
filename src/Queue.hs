{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Queue where

import           Control.Monad.IO.Class      (liftIO)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BL
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as T
import           Database.Persist.Postgresql
import qualified Network.AMQP                as AMQP

import qualified Models                      as DM
import           Utils.Controller            (HandlerT, taskPubConnection)

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

submitTask
  :: Key DM.Submit
  -> [(T.Text, BS.ByteString)]
  -> HandlerT IO (Maybe AMQP.ConfirmationResult)
submitTask submitKey filesContents = do
  connection <- asks taskPubConnection
  $logDebug $ "Publishing task " <> (T.pack . show) task
  publishResult <-
    liftIO $ withTaskPublisher connection $ \publisher -> publish publisher task
  $logDebug $ "Publish result: " <> (T.pack . show) publishResult
  return publishResult
  where
    taskId = fromSqlKey submitKey
    config = "dummy config"
    tests =
      [ Test "0\n" "0\n2\n4\n" 0 0
      , Test "1\n" "1\n3\n5\n" 0 0
      ]
    taskFiles =
      map
        (\(filename, contents) -> File filename $ BL.fromStrict contents)
        filesContents
    task = Task taskId config taskFiles tests
