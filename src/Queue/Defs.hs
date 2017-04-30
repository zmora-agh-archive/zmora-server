{-# LANGUAGE OverloadedStrings #-}

module Queue.Defs where

import           Network.AMQP
import           Queue.Publisher
import           Queue.Serialization (defaultSerializer)
import           Models.Task

--
-- Standard publisher options
--
taskPublisherOpts :: PublisherOpts Task
taskPublisherOpts = PublisherOpts defaultConnectionOpts exchange defaultSerializer
  where exchange = newExchange {
            exchangeName = "tasks"
          , exchangeType = "fanout"
          }

