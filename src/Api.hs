{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}

module Api where

import Data.Int         (Int64(..))
import Database.Persist (Entity(..))
import Servant

import Utils.ResourceAPI
import Models

type StdActions a = '[ Get '[JSON] [Entity a]
                     , Capture "id" Int64 :> Get '[JSON] a
                     ]

type API = ResourceAPI '[
    Resource "time" '[Get '[JSON] CurrentTime] '[]
  , Resource "users" (StdActions User) '[]
  , Resource "contests" (StdActions Contest) '[
      Resource "problems" '[ Get '[JSON] [ContestProblem]
                           , Capture "id" Int64 :> Get '[JSON] ContestProblem
                           ] '[
          Resource "examples"  '[Get '[JSON] [Entity ProblemExample]] '[]
        , Resource "questions" '[Get '[JSON] [Entity Question]] '[]
        , Resource "submits" (StdActions Submit) '[]
      ]
    ]
  ]

api :: Proxy API
api = Proxy
