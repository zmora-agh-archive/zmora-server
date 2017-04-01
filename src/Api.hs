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

type ProtectedAPI = ResourceAPI '[
    Resource "time" '[Get '[JSON] CurrentTime] '[]
  , Resource "users" '[Capture "id" Int64 :> Get '[JSON] User] '[]
  , Resource "contests" '[
        Get '[JSON] [Entity' Contest ContestWithOwners]
      , Capture "id" Int64 :> Get '[JSON] ContestWithOwners
    ] '[
      Resource "problems" '[ Get '[JSON] [ExpandedContestProblem]
                           , Capture "id" Int64 :> Get '[JSON] Problem
                           ] '[
          Resource "examples"  '[Get '[JSON] [ProblemExampleWithoutProblem]] '[]
        , Resource "questions" '[Get '[JSON] [QuestionWithAnswers]] '[]
        , Resource "submits" '[
              Get '[JSON] [Entity' Submit SubmitWithoutAuthor]
            , Capture "id" Int64 :> Get '[JSON] SubmitWithoutAuthor
          ] '[]
      ]
    ]
  ]

type API = (Auth '[JWT] CurrentUser :> ProtectedAPI) :<|> PublicAPI

api :: Proxy API
api = Proxy
