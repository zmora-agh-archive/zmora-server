{-# LANGUAGE OverloadedStrings #-}

module Queue.AMQP where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import           Network.AMQP

data Worker = Worker {workerConnection :: Connection, workerChannel :: Channel}

connect :: ConnectionOpts -> IO Worker
connect opts = do
  conn <- openConnection'' opts
  chan <- openChannel conn
  return $ Worker conn chan

disconnect :: Worker -> IO ()
disconnect (Worker conn _) = closeConnection conn

data Publisher m = Publisher (Maybe ExchangeOpts) (m -> BS.ByteString) Worker
data Subscriber m = Subscriber QueueOpts (BS.ByteString -> IO m) Worker

connectPublisher :: ConnectionOpts -> Maybe ExchangeOpts -> (m -> BS.ByteString) -> IO (Publisher m)
connectPublisher cOpts eOpts serializer = do
  worker <- connect cOpts
  maybe (return ()) (declareExchange $ workerChannel worker) eOpts
  return $ Publisher eOpts serializer worker

connectSubscriber :: ConnectionOpts -> QueueOpts -> (BS.ByteString -> IO m) -> IO (Subscriber m)
connectSubscriber cOpts qOpts deserializer = do
  worker <- connect cOpts
  _ <- declareQueue (workerChannel worker) qOpts
  return $ Subscriber qOpts deserializer worker

publish :: Publisher m -> T.Text -> m -> IO (Maybe Int)
publish (Publisher eOpts serializer (Worker _ channel)) key msg =
  publishMsg channel exchange key newMsg {msgBody = serializer msg}
  where exchange = maybe "" exchangeName eOpts

withPublisher :: ConnectionOpts -> Maybe ExchangeOpts -> (m -> BS.ByteString) -> (Publisher m -> IO ()) -> IO ()
withPublisher cOpts eOpts serializer f = do
  publisher <- connectPublisher cOpts eOpts serializer
  (Publisher _ _ worker) <- return publisher
  f publisher
  disconnect worker

subscribe :: Subscriber m -> ((m, Envelope) -> IO ()) -> IO ConsumerTag
subscribe (Subscriber qOpts deserializer (Worker _ channel)) f =
  consumeMsgs channel (queueName qOpts) Ack (\(msg, env) -> do
                                                x <- deserializer $ msgBody msg
                                                f (x, env))
