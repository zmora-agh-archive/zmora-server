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

module Models where

import Control.Lens
import Data.Aeson
import Data.Hashable
import Data.Hashable.Time
import GHC.Generics
import Data.Aeson.TH
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sql
import Data.Text
import Data.ByteString
import Data.Time.Clock as T

import Models.Group
import Utils.AesonTrim
import Utils.ExtensibleRecords

newtype CurrentTime = CurrentTime { _CurrentTimeTime :: UTCTime }
makeLenses ''CurrentTime
deriveJSON (defaultOptionsWithTrim "_CurrentTime") ''CurrentTime

-- Important notice: All nodes must have timezone set to UTC
getCurrentTime :: IO CurrentTime
getCurrentTime = CurrentTime <$> T.getCurrentTime

instance ( ToBackendKey SqlBackend a, Hashable a
         ) => Hashable (Entity a) where
  hashWithSalt salt (Entity k v) =
    salt `hashWithSalt` fromSqlKey k `hashWithSalt` v

instance ToBackendKey SqlBackend a => Hashable (Key a) where
  hashWithSalt salt k = salt `hashWithSalt` fromSqlKey k

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

ContestProblem json
  shortcode Text
  contest ContestId
  problem ProblemId
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
  checksum Text
  deriving Show

SubmitFile
  submit SubmitId
  contest ByteString
  deriving Show
|]

instance Hashable User
instance Hashable Contest
instance Hashable ContestProblem
instance Hashable Question

--
-- Model derivates
--

newtype ContestWithOwners = ContestWithOwners {
  _contestWithOwners ::  AsRec Contest
                      :+ "id" :-> Key Contest
                      :+ "owners" :-> [User]
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
                           :+ "id" :-> Key ContestProblem
                           :- "contest"
} deriving Show

instance ToJSON ExpandedContestProblem where
  toJSON = toJSON . _expandedContestProblem

newtype ProblemExampleWithoutProblem = ProblemExampleWithoutProblem {
  _problemExampleWithoutProblemId :: AsRec ProblemExample
                                   :- "problem"
                                   :- "number"
} deriving Show

instance ToJSON ProblemExampleWithoutProblem where
  toJSON = toJSON . _problemExampleWithoutProblemId

newtype QuestionWithAnswers = QuestionWithAnswers {
  _questionWithAnswers ::  AsRec Question
                        :% "author" :-> User
                        :+ "answers" :-> [AnswerWithoutQuestion]
                        :- "problem"
} deriving Show

instance ToJSON QuestionWithAnswers where
  toJSON = toJSON . _questionWithAnswers

newtype AnswerWithoutQuestion = AnswerWithoutQuestion {
  _answerWithoutQuestion ::  AsRec Answer
                          :% "author" :-> User
                          :- "question"
} deriving Show

instance ToJSON AnswerWithoutQuestion where
  toJSON = toJSON . _answerWithoutQuestion
