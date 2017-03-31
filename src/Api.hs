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

type StdActions a = '[ Get '[JSON] [Entity a]
                     , Capture "id" Int64 :> Get '[JSON] a
                     ]

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
        Get '[JSON] [ContestWithOwners]
      , Capture "id" Int64 :> Get '[JSON] Contest
    ] '[
      Resource "problems" '[ Get '[JSON] [ExpandedContestProblem]
                           , Capture "id" Int64 :> Get '[JSON] Problem
                           ] '[
          Resource "examples"  '[Get '[JSON] [ProblemExampleWithoutProblem]] '[]
        , Resource "questions" '[Get '[JSON] [QuestionWithAnswers]] '[]
        , Resource "submits" (StdActions Submit) '[]
      ]
    ]
  ]

type API = (Auth '[JWT] CurrentUser :> ProtectedAPI) :<|> PublicAPI

api :: Proxy API
api = Proxy
