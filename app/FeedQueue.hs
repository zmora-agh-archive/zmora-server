{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Models.Task
import           Network.AMQP (fromURI)
import           Queue.AMQP
import           Queue.Defs

testTask :: Task
testTask = Task 1 "dummy config string" [File "source.c" "void main() {}"] []

brokerURI :: String
brokerURI = "amqp://guest:guest@localhost:5672"

main :: IO ()
main = do
  putStrLn $ "Sending task " ++ show testTask
  withConnection (fromURI brokerURI) $ \connection ->
    withTaskPublisher connection $ \publisher -> do
      result <- publish publisher testTask
      putStrLn $ "(acks, nacks, [pending]): " ++ show result
