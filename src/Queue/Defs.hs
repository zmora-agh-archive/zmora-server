{-# LANGUAGE OverloadedStrings #-}

module Queue.Defs where

import           Models.Task
import           Network.AMQP
import           Queue.AMQP
import           Queue.Serialization

withTaskPublisher :: Connection -> (Publisher Task -> IO a) -> IO a
withTaskPublisher connection = withPublisher connection spec
  where
    spec =
      PublisherSpec
      { pubExchangeOpts = Nothing
      , pubKey = "tasks"
      , pubSerializer = defaultSerializer
      , pubAwaitNanos = Just 1000000
      }

taskResultSubscriber :: Connection -> IO (Subscriber TaskResult)
taskResultSubscriber connection = openChannel connection >>= newSubscriber spec
  where
    spec =
      SubscriberSpec (newQueue {queueName = "tasksResults"}) defaultDeserializer
