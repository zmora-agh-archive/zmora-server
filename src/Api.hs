{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}

module Api where

import Data.Int         (Int64(..))
import Database.Persist (Entity(..))
import Servant
import Servant.Auth.Server

import Utils.ResourceAPI
import Utils.ExtensibleRecords
import Models

type PublicAPI = ResourceAPI '[
    Resource "users" '[
        ReqBody '[JSON] UserRegistration :> Post '[JSON] (Key User)
      , "auth" :> ReqBody '[JSON] Login :> Post '[JSON] JwtToken
    ] '[]
  ]

type StdActions a = '[Get '[JSON] [a], Capture "id" Int64 :> Get '[JSON] a]

type ProtectedAPI = ResourceAPI '[
    Resource "time" '[Get '[JSON] CurrentTime] '[]
  , Resource "contests" (StdActions (Entity' Contest ContestWithOwners)) '[
        Resource "problems" (StdActions (Entity' ContestProblem ExpandedContestProblem)) '[
          Resource "examples"  '[Get '[JSON] [ProblemExample]] '[]
        , Resource "questions" '[Get '[JSON] [QuestionWithAnswers]] '[]
        , Resource "submits" (StdActions (Entity Submit)) '[]
      ]
    ]
  ]

type API = (Auth '[JWT] CurrentUser :> ProtectedAPI) :<|> PublicAPI

api :: Proxy API
api = Proxy
