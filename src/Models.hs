{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Models where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString
import Data.Hashable
import Data.Hashable.Time
import Data.HashMap.Strict as HM
import Data.Text
import Data.Time.Clock as T
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics

import Models.Group
import Utils.AesonTrim
import Utils.ExtensibleRecords

newtype CurrentTime = CurrentTime { _currentTimeTime :: UTCTime }
makeLenses ''CurrentTime
deriveJSON (defaultOptionsWithTrim "_currentTime") ''CurrentTime

-- Important notice: All nodes must have timezone set to UTC
getCurrentTime :: IO CurrentTime
getCurrentTime = CurrentTime <$> T.getCurrentTime

instance ( ToBackendKey SqlBackend a, Hashable a
         ) => Hashable (Entity a) where
  hashWithSalt salt (Entity k v) =
    salt `hashWithSalt` fromSqlKey k `hashWithSalt` v

instance ToBackendKey SqlBackend a => Hashable (Key a) where
  hashWithSalt salt k = salt `hashWithSalt` fromSqlKey k

submitStatusQUE = "QUE" :: Text
submitStatusERR = "ERR" :: Text
submitStatusOK = "OK" :: Text


share
  [ mkPersist sqlSettings { mpsGenerateLenses = True }
  , mkMigrate "migrateAll"
  , mkExtensibleRecords
  ] [persistLowerCase|
Credential
  email Text
  salt ByteString
  hash ByteString
  user UserId
  group Group
  UniqueEmail email
  UniqueUser user
  deriving Show

User json
  nick Text
  name Text
  about Text
  UniqueNick nick
  deriving Show
  deriving Eq
  deriving Generic

UserAvatar
  user UserId
  avatar ByteString
  UniqueAvatar user
  deriving Show

ContestOwnership json
  contest ContestId
  owner UserId
  joinPassword Text
  UniqueOwnership contest owner
  UniquePassword contest joinPassword
  deriving Show

Contest json
  name Text
  description Text
  start UTCTime
  signupDuration Int
  duration Int
  deriving Show
  deriving Eq
  deriving Generic

Problem json
  author UserId
  name Text
  description Text
  deriving Show

ProblemExample json
  problem ProblemId
  number Int
  input Text
  result Text
  explanation Text
  deriving Show

ContestParticipation json
  contest ContestId
  user UserId
  UniqueParticipation contest user
  deriving Show
  deriving Generic
  deriving Eq

ContestProblem json
  contest ContestId
  problem ProblemId
  shortcode Text
  category Text
  basePoints Int
  softDeadline UTCTime
  hardDeadline UTCTime maybe
  required Bool
  UniqueProblem contest problem
  UniqueShortcode contest shortcode
  deriving Show
  deriving Generic

Question json
  problem ContestProblemId
  author UserId
  question Text
  asked UTCTime
  deriving Show
  deriving Eq
  deriving Generic

Answer json
  question QuestionId
  author UserId
  answered UTCTime
  answer Text
  UniqueAnswer question author
  deriving Show

Submit json
  problem ContestProblemId
  author UserId
  date UTCTime
  status Text
  deriving Show
  deriving Eq
  deriving Generic

TestResult json
  submit SubmitId
  status Text
  executionTime Int
  ramUsage Int
  deriving Show

SubmitFile
  submit SubmitId
  file ByteString
  checksum Text
  filename Text
  deriving Show
  deriving Eq
  deriving Generic
|]

instance Hashable Contest
instance Hashable ContestParticipation
instance Hashable ContestProblem
instance Hashable Question
instance Hashable User
instance Hashable Submit
instance Hashable SubmitFile

--
-- Auth logic
--
type CurrentUser = Entity User

data Login = Login { _nick :: Text
                   , _password :: Text
                   } deriving Show

makeLenses ''Login
deriveJSON (defaultOptionsWithTrim "_") ''Login

newtype JwtToken = JwtToken { token :: Text } deriving Show
deriveJSON defaultOptions ''JwtToken
--
-- Model derivates
--

data Entity' a b = Entity' { entityKey' :: Key a
                           , entityVal' :: b
                           }

deriving instance (Show (Key a), Show b) => Show (Entity' a b)

instance (ToJSON (Key a), ToJSON b) => ToJSON (Entity' a b) where
  toJSON (Entity' key value) = case toJSON value of
    Object o -> Object $ HM.insert "id" (toJSON key) o
    x -> x


newtype ContestWithOwners = ContestWithOwners {
  _contestWithOwners ::  AsRec Contest
                      :+ "owners" :-> [User]
                      :+ "joined" :-> Bool
} deriving Show

instance ToJSON ContestWithOwners where
  toJSON = toJSON . _contestWithOwners

newtype UserRegistration = UserRegistration {
  _userRegistration ::  AsRec User
                     :+ "email"    :-> Text
                     :+ "password" :-> Text
} deriving Show

instance FromJSON UserRegistration where
  parseJSON ov = UserRegistration <$> parseJSON ov

newtype ExpandedContestProblem = ExpandedContestProblem {
  _expandedContestProblem ::  AsRec ContestProblem
                           :% "problem" :-> Problem
} deriving Show

instance ToJSON ExpandedContestProblem where
  toJSON = toJSON . _expandedContestProblem

newtype QuestionWithAnswers = QuestionWithAnswers {
  _questionWithAnswers ::  AsRec Question
                        :% "author" :-> User
                        :+ "answers" :-> [ExpandedAnswer]
} deriving Show

instance ToJSON QuestionWithAnswers where
  toJSON = toJSON . _questionWithAnswers

newtype ExpandedAnswer = ExpandedAnswer {
  _expandedAnswer :: AsRec Answer :% "author" :-> User
} deriving Show

instance ToJSON ExpandedAnswer where
  toJSON = toJSON . _expandedAnswer

newtype SubmitWithFiles = SubmitWithFiles {
  _submitWithFiles :: AsRec Submit
                    :+ "files" :-> [Entity' SubmitFile (AsRec SubmitFile :- "file")]
} deriving Show

instance ToJSON SubmitWithFiles where
  toJSON = toJSON . _submitWithFiles

newtype SubmitWithFilesAndTests = SubmitWithFilesAndTests {
  _submitWithFilesAndTests :: AsRec Submit
                    :+ "tests" :-> [Entity' TestResult (AsRec TestResult :- "submit")]
                    :+ "files" :-> [Entity' SubmitFile (AsRec SubmitFile :- "file" :- "submit")]
} deriving Show

instance ToJSON SubmitWithFilesAndTests where
  toJSON = toJSON . _submitWithFilesAndTests
