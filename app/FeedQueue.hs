{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad (void)
import           Models.Task
import           Network.AMQP  (waitForConfirms)
import           Queue.AMQP
import           Queue.Defs

testTask :: Task
testTask = Task 1 "dummy config string" [File "source.c" "void main() {}"] []

main :: IO ()
main = withTaskPublisher $ \publisher -> do
  Publisher _ _ (Worker _ chan) <- return publisher
  _ <- publish publisher "tasks" testTask
  void $ waitForConfirms chan
