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

Question json
  problem ContestProblemId
  author UserId
  question Text
  answer AnswerId Maybe

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
|]

newtype ContestWithOwners = ContestWithOwners {
  _contestWithOwners ::  AsRec Contest
                      :+ "id" :-> (Key Contest)
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

instance ( ToBackendKey SqlBackend a, Hashable a
         ) => Hashable (Entity a) where
  hashWithSalt salt (Entity k v) =
    salt `hashWithSalt` fromSqlKey k `hashWithSalt` v

instance Hashable Contest

-- TODO TemplateHaskell this boilerplate
instance RecImplode User where
  type ImplCtx User a = ( RecGetProp "nick" a Text
                        , RecGetProp "name" a Text
                        , RecGetProp "about" a Text
                        )
  implode = User <$> rGet (Var :: Var "nick")
                 <*> rGet (Var :: Var "name")
                 <*> rGet (Var :: Var "about")
