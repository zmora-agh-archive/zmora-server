{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Models.Task
import           Queue.AMQP
import           Queue.Defs

testTask :: Task
testTask = Task 1 "dummy config string" [File "source.c" "void main() {}"] []

main :: IO ()
main = do
  putStrLn $ "Sending task " ++ show testTask
  withConnection connectionOpts $ \connection ->
    withTaskPublisher connection $ \publisher -> do
      result <- publish publisher testTask
      putStrLn $ "(acks, nacks, [pending]): " ++ show result
