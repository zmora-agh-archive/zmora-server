module Models.Task where

import qualified Data.ByteString.Lazy as B

data File = File {
  name    :: String,
  content :: B.ByteString
} deriving Show

data Test = Test {
  input     :: B.ByteString,
  output    :: B.ByteString,
  timeLimit :: Int,
  ramLimit  :: Int
} deriving Show

data Task = Task {
    taskId        :: Int,
    configuration :: String,
    files         :: [File],
    tests         :: [Test]
} deriving Show

data TestResult = TestResult {
  passed        :: Bool,
  executionTime :: Int,
  ramUsage      :: Int
} deriving Show

data TaskResult = TaskResult {
  resultId       :: Int,
  compilationLog :: String,
  testResults    :: [TestResult]
} deriving Show
