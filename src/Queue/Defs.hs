{-# LANGUAGE OverloadedStrings #-}

module Queue.Defs where

import           Models.Task
import           Network.AMQP
import           Queue.AMQP
import           Queue.Serialization

connectionOpts :: ConnectionOpts
connectionOpts = fromURI "amqp://guest:guest@localhost:5672"

withTaskPublisher :: (Publisher Task -> IO ()) -> IO ()
withTaskPublisher = withPublisher connectionOpts Nothing defaultSerializer

taskResultSubscriber :: IO (Subscriber TaskResult)
taskResultSubscriber = connectSubscriber connectionOpts queueOpts defaultDeserializer
  where queueOpts = newQueue {queueName = "tasksResults"}
