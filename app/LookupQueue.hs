{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad       (unless)
import           Data.MessagePack
import qualified Data.Text           as T
import           Models.Task
import           Network.AMQP
import           Queue.AMQP
import           Queue.Serialization

handleMessage :: (MessagePack a, Show a) => Bool -> (a, Envelope) -> IO ()
handleMessage requeue (msg, envelope) = do
  putStrLn $ "Received message: " ++ show msg
  unless requeue $ ackEnv envelope

standardSubscriber :: MessagePack a => T.Text -> IO (Subscriber a)
standardSubscriber queueName =
  connectSubscriber connOpts queue defaultDeserializer
  where connOpts = fromURI brokerURI
        queue = newQueue {queueName, queuePassive = True}

subscriberQueueName :: Subscriber a -> String
subscriberQueueName (Subscriber qOpts _ _) = T.unpack . queueName $ qOpts

taskSubscriber :: IO (Subscriber Task)
taskSubscriber = standardSubscriber "tasks"
taskResultSubscriber :: IO (Subscriber TaskResult)
taskResultSubscriber = standardSubscriber "tasksResults"

brokerURI :: String
brokerURI = "amqp://guest:guest@localhost:5672"

main :: IO ()
main = let
  requeue = True
  subscriber = taskResultSubscriber
  in do
    queueName <- subscriberQueueName <$> subscriber
    putStrLn $ "Using URI: " ++ brokerURI
    putStrLn $ "Subscribe queue: " ++ queueName ++ " (requeue: " ++ show requeue ++ ")"
    putStrLn "Press RETURN to exit..."

    _ <- subscriber >>= flip subscribe (handleMessage requeue)

    _ <- getLine
    return ()
