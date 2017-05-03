{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.MessagePack
import qualified Data.Text           as T
import           Models.Task
import           Network.AMQP
import           Queue.AMQP
import           Queue.Serialization

handleMessage
  :: (MessagePack a, Show a)
  => Bool -> (a, Envelope) -> IO ()
handleMessage requeue (msg, envelope) = do
  putStrLn $ "Received message: " ++ show msg
  if requeue
    then rejectEnv envelope True
    else ackEnv envelope

standardSubscriber
  :: MessagePack a
  => T.Text -> Connection -> IO (Subscriber a)
standardSubscriber queueName connection =
  openChannel connection >>= newSubscriber spec
  where
    spec = SubscriberSpec queue defaultDeserializer
    queue = newQueue {queueName, queuePassive = True}

subscriberQueueName :: Subscriber a -> String
subscriberQueueName (Subscriber spec _) = T.unpack . queueName . subOpts $ spec

taskSubscriber :: Connection -> IO (Subscriber Task)
taskSubscriber = standardSubscriber "tasks"

taskResultSubscriber :: Connection -> IO (Subscriber TaskResult)
taskResultSubscriber = standardSubscriber "tasksResults"

brokerURI :: String
brokerURI = "amqp://guest:guest@localhost:5672"

main :: IO ()
main = do
  putStrLn $ "Using options: " ++ brokerURI
  putStrLn "Press RETURN to EXIT GRACEFULLY..."
  withConnection (fromURI brokerURI) $ \connection -> do
    _ <- taskSubscriber connection >>= flip subscribe (handleMessage True)
    _ <- taskResultSubscriber connection >>= flip subscribe (handleMessage True)
    _ <- getLine
    return ()
