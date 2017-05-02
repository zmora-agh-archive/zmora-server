{-# LANGUAGE OverloadedStrings #-}

module Queue.AMQP where

import           Control.Monad        ()
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text            as T
import           Network.AMQP

connect :: ConnectionOpts -> IO Channel
connect connectionOpts = openConnection'' connectionOpts >>= openChannel

withConnection :: ConnectionOpts -> (Connection -> IO a) -> IO a
withConnection opts f = do
  connection <- openConnection'' opts
  res <- f connection
  closeConnection connection
  return res

data PublisherSpec m = PublisherSpec
  { pubExchangeOpts :: Maybe ExchangeOpts
  , pubKey          :: T.Text
  , pubSerializer   :: m -> BS.ByteString
  , pubAwaitNanos   :: Maybe Int
  }

data Publisher m =
  Publisher (PublisherSpec m)
            Channel

data SubscriberSpec m = SubscriberSpec
  { subOpts         :: QueueOpts
  , subDeserializer :: BS.ByteString -> IO m
  }

data Subscriber m =
  Subscriber (SubscriberSpec m)
             Channel

newPublisher :: PublisherSpec m -> Channel -> IO (Publisher m)
newPublisher spec channel = do
  mapM_ (declareExchange channel) (pubExchangeOpts spec)
  return $ Publisher spec channel

connectPublisher :: ConnectionOpts -> PublisherSpec m -> IO (Publisher m)
connectPublisher opts spec = connect opts >>= newPublisher spec

newSubscriber :: SubscriberSpec m -> Channel -> IO (Subscriber m)
newSubscriber spec channel = do
  _ <- declareQueue channel (subOpts spec)
  return $ Subscriber spec channel

connectSubscriber :: ConnectionOpts -> SubscriberSpec m -> IO (Subscriber m)
connectSubscriber opts spec = connect opts >>= newSubscriber spec

publish :: Publisher m -> m -> IO (Maybe ConfirmationResult)
publish (Publisher (PublisherSpec opts key serializer awaitNanos) channel) msg = do
  _ <- publishMsg
       channel
       (maybe "" exchangeName opts)
       key
       newMsg {msgBody = serializer msg}
  mapM (waitForConfirmsUntil channel) awaitNanos

withPublisher :: Connection -> PublisherSpec m -> (Publisher m -> IO a) -> IO a
withPublisher connection spec f = do
  channel <- openChannel connection
  publisher <- newPublisher spec channel
  res <- f publisher
  closeChannel channel
  return res

subscribe :: Subscriber m -> ((m, Envelope) -> IO ()) -> IO ConsumerTag
subscribe (Subscriber (SubscriberSpec opts deserializer) channel) f =
  consumeMsgs
    channel
    (queueName opts)
    Ack
    (\(msg, env) -> do
       x <- deserializer $ msgBody msg
       f (x, env))
