module Models.Task where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import Data.Int

data Task = Task
  { taskId :: Int64
  , configuration :: String
  , files :: [File]
  , tests :: [Test]
  } deriving (Show)

data File = File
  { name :: T.Text
  , content :: B.ByteString
  } deriving (Show)

data Test = Test
  { input :: String
  , output :: String
  , timeLimit :: Int
  , ramLimit :: Int
  } deriving (Show)

data TaskResult = TaskResult
  { resultId :: Int64
  , compilationLog :: String
  , testResults :: [TestResult]
  } deriving (Show)

data TestResult = TestResult
  { status :: Status
  , executionTime :: Int
  , ramUsage :: Int
  } deriving (Show)

data Status
  = OK
  | RTE
  | MEM
  | TLE
  | ANS
  | CME
  deriving (Show)
