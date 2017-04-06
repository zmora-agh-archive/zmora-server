{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE FlexibleInstances    #-}

module Api where

import Data.Int         (Int64(..))
import Database.Persist (Entity(..))
import Servant
import Servant.Auth.Server

import Utils.ExtensibleRecords
import Models

type G = Get  '[JSON]
type P = Post '[JSON]

type PublicAPI =
       "users" :> ReqBody '[JSON] UserRegistration :> P (Key User)
  :<|> "users" :> "auth" :> ReqBody '[JSON] Login :> P JwtToken

type ContestPath a = "contests" :> Capture "id" (Key Contest) :> a
type ContestProblemPath a = ContestPath ("problems" :> Capture "id" (Key ContestProblem) :> a)
type ContestProblemSubmitPath a = ContestProblemPath ("submits" :> Capture "id" (Key Submit) :> a)

type ProtectedAPI =
       "time"     :> G CurrentTime

  :<|> "contests"  :> G [Entity' Contest ContestWithOwners]
  :<|> ContestPath ( G (Entity' Contest ContestWithOwners) )

  :<|> ContestPath ( "problems" :> G [Entity' ContestProblem ExpandedContestProblem] )
  :<|> ContestProblemPath ( G (Entity' ContestProblem ExpandedContestProblem) )

  :<|> ContestProblemPath ( "examples"  :> G [ProblemExample] )
  :<|> ContestProblemPath ( "questions" :> G [QuestionWithAnswers] )
  :<|> ContestProblemPath ( "submits"   :> G [Entity Submit] )
  :<|> ContestProblemSubmitPath ( G (Entity Submit) )

type API = (Auth '[JWT] CurrentUser :> ProtectedAPI) :<|> PublicAPI

api :: Proxy API
api = Proxy
