{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeFamilies        #-}

module Api
    ( startApp
    ) where

import Network.Wai
import Network.Wai.Middleware.Cors
import Network.Wai.Handler.Warp
import Database.Persist ( Entity(..) )
import GHC.Types

import Servant
import Servant.Mock

import Models
import Fake

type family ResourceAPI (path :: Symbol) a where
  ResourceAPI path a =  path :> Get '[JSON] [Entity a]
                   :<|> path :> Capture "id" Integer :> Get '[JSON] a

type family SResourceAPI (path :: Symbol) a sub where
  SResourceAPI path a sub = path :> Get '[JSON] [Entity a]
    :<|> path :> Capture "id" Integer :> (Get '[JSON] a :<|> sub)

type API = "currentUser" :> Get '[JSON] (Entity User) -- temporary
       :<|> "getTime" :> Get '[JSON] CurrentTime
       :<|> ResourceAPI "users" User
       :<|> SResourceAPI "contests" Contest (
              SResourceAPI "problems" ContestProblem (
                     ResourceAPI "examples" ProblemExample
                :<|> ResourceAPI "questions" Question
                :<|> ResourceAPI "submits" Submit
              )
            )

api :: Proxy API
api = Proxy

startApp :: IO ()
startApp = run 8080 $ simpleCors $ serve api (mock api Proxy)
