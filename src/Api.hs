{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds            #-}

module Api where

import Database.Persist ( Entity(..) )
import Data.Int (Int64(..))
import GHC.Types
import Servant

import Models

data Resource :: Symbol -> * -> Symbol -> [*] -> *

type ResourceDecl = '[
      Resource "users" User "user_id" '[]
    , Resource "contests" Contest "contest_id" '[
        Resource "problems" ContestProblem "contest_problem_id" '[
            Resource "examples" ProblemExample "problem_example_id" '[]
          , Resource "questions" Question "problem_question_id" '[]
          , Resource "submits" Submit "problem_submit_id" '[]
        ]
      ]
  ]

type family (++) (as :: [k]) (bs :: [k]) :: [k] where
  (++) a '[] = a
  (++) '[] b = b
  (++) (a ': as) bs = a ': (as ++ bs)

type family ResPaths lvl res where
  ResPaths lvl (Resource path a id '[]) = '[lvl ++ '[('(path, a, id))]]
  ResPaths lvl (Resource path a id (x ': xs)) =
       (ResPaths (lvl ++ '[('(path, a, id))]) x)
    ++ (ResPaths lvl (Resource path a id xs))

type family ResPathAPI spec where
  ResPathAPI ('(path, a, id) ': '[]) =  path :> Get '[JSON] [Entity a]
                                   :<|> path :> Capture id Int64 :> Get '[JSON] a
  ResPathAPI ('(path, a, id) ': xs) = path :> Capture id Int64 :> ResPathAPI xs

type family DeclMap l where
  DeclMap (x ': '[]) = ResPaths '[] x
  DeclMap (x ':  xs) = ResPaths '[] x ++ DeclMap xs

type family ResourceAPI spec where
  ResourceAPI (x ': '[]) = ResPathAPI x
  ResourceAPI (x ':  xs) = ResPathAPI x :<|> ResourceAPI xs

type API = ResourceAPI (DeclMap ResourceDecl)

api :: Proxy API
api = Proxy
