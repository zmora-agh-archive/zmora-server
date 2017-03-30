module Models.Derivates where

import Utils.ExtensibleRecords

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
                           :% "problem" :-> Entity Problem
                           :- "contest"
} deriving Show

instance ToJSON ExpandedContestProblem where
  toJSON = toJSON . _expandedContestProblem

newtype ProblemExampleWithoutProblemId = ProblemExampleWithoutProblemId {
  _problemExampleWithoutProblemId :: AsRec ContestProblem :- "problem"
} deriving Show

instance ToJSON ProblemExampleWithoutProblem where
  toJSON = toJSON . _problemExampleWithoutProblemId
