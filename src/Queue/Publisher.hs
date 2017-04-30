{-# LANGUAGE OverloadedStrings #-}

module Queue.Publisher where

import qualified Data.ByteString.Lazy as BS
import qualified Data.MessagePack as MP
import           Network.AMQP

import           Models.Task
import           Queue.Serialization

data PublisherOpts m = PublisherOpts {
    publishConnOpts     :: ConnectionOpts
  , publishExchangeOpts :: ExchangeOpts
  , publishSerializer :: m -> BS.ByteString
  }

data Publisher m = Publisher {
    publisherOpts :: PublisherOpts m
  , conn :: Connection
  , chan :: Channel
  }

connectPublisher :: PublisherOpts m -> IO (Publisher m)
connectPublisher opts = do
  conn <- openConnection'' . publishConnOpts $ opts
  chan <- openChannel conn
  return $ Publisher opts conn chan

disconnectPublisher :: (Publisher m) -> IO ()
disconnectPublisher = closeConnection . conn

publish :: Publisher m -> m -> IO (Maybe Int)
publish publisher msg =
  publishMsg channel exchange "key" newMsg {msgBody = serialize msg}
  where
    channel = chan publisher
    exchange = exchangeName . publishExchangeOpts . publisherOpts $ publisher
    serialize = publishSerializer . publisherOpts $ publisher

withPublisher :: PublisherOpts m -> (Publisher m -> IO a) -> IO a
withPublisher opts f = do
  p <- connectPublisher opts
  res <- f p
  closeConnection (conn p)
  return res

