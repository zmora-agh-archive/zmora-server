{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import Control.Lens
import Data.Aeson.TH
import Data.ByteString
import Data.Int (Int64)
import qualified Data.Char as C
import Data.Text
import Data.Time.Clock
import Database.Persist
import Database.Persist.TH

import Models.Group

share
  [ mkPersist sqlSettings { mpsGenerateLenses = True }
  , mkMigrate "migrateAll"
  ] [persistLowerCase|
Credential
  email Text
  salt ByteString
  hash ByteString
  user User
  group Group
  UniqueEmail email
  UniqueUser user
  deriving Show

User json
  nick Text
  name Text
  avatar Text
  about Text
  UniqueNick nick
  deriving Show

ContestOwnership json
  contest ContestId
  owner UserId
  joinPassword Text
  deriving Show

Contest json
  name Text
  description Text
  start UTCTime
  signupDuration Int
  duration Int
  deriving Show

Problem json
  author UserId
  name Text
  description Text
  deriving Show

ProblemExample json
  problem ProblemId
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
  problem Problem
  UniqueProblem contest problem
  UniqueShortcode contest shortcode
  deriving Show

Question json
  problem ContestProblemId
  author UserId
  question Text
  answer Answer Maybe

Answer json
  author UserId
  answer Text

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

CurrentTime json
  time UTCTime
|]

data ContestWithOwners = ContestWithOwners {
      _contestWithOwnersId :: Key Contest
    , _contestWithOwnersName :: Text
    , _contestWithOwnersDescription :: Text
    , _contestWithOwnersStart :: UTCTime
    , _contestWithOwnersSignupDuration :: Int
    , _contestWithOwnersDuration :: Int
    , _contestWithOwnersOwners :: [User]
  } deriving Show

makeLenses ''ContestWithOwners
deriveJSON defaultOptions{fieldLabelModifier = over _head C.toLower . Prelude.drop 18}
           ''ContestWithOwners
