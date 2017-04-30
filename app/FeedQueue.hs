{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BS

import           Queue.Publisher
import           Queue.Serialization
import           Models.Task
import           Queue.Defs

testTask :: Task
testTask = Task 1 "none" [File "test.c" "contents"] []

main = withPublisher taskPublisherOpts $ \publisher ->
  publish publisher testTask
  -- confirmSelect chan True
  -- conf <- waitForConfirmsUntil chan 1000000
