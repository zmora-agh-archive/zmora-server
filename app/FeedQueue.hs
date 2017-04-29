{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BL
import           Network.AMQP

data PublisherOpts = PublisherOpts {
    publishConnOpts     :: ConnectionOpts
  , publishExchangeOpts :: ExchangeOpts
  }

data Publisher = Publisher {publisherOpts :: PublisherOpts, conn :: Connection, chan :: Channel}

taskPublisherOpts = PublisherOpts defaultConnectionOpts newExchange {
    exchangeName = "tasks"
  , exchangeType = "fanout"
  }

connectPublisher :: PublisherOpts -> IO Publisher
connectPublisher opts = do
  conn <- openConnection'' . publishConnOpts $ opts
  chan <- openChannel conn
  return $ Publisher opts conn chan

disconnectPublisher :: Publisher -> IO ()
disconnectPublisher = closeConnection . conn

publishTask :: Publisher -> [Char] -> IO (Maybe Int)
publishTask publisher msg =
  publishMsg channel exchange "key" newMsg {msgBody = BL.pack msg}
  where
    channel = chan publisher
    exchange = exchangeName . publishExchangeOpts . publisherOpts $ publisher

main = do
  taskPublisher <- connectPublisher taskPublisherOpts
  publishTask taskPublisher "Hello, world!"
  disconnectPublisher taskPublisher
  -- confirmSelect chan True
  -- conf <- waitForConfirmsUntil chan 1000000
