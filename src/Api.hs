{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeOperators       #-}

module Api
    ( startApp
    ) where

import Network.Wai
import Network.Wai.Middleware.Cors
import Network.Wai.Handler.Warp
import Database.Persist ( Entity(..) )

import Servant
import Servant.Mock

import Models
import Fake

type API =  "users"    :> Get '[JSON] [Entity User]
       :<|> "contests" :> Capture "id" Integer :> Get '[JSON] Contest
       :<|> "contests" :> Get '[JSON] [Entity Contest]

api :: Proxy API
api = Proxy

startApp :: IO ()
startApp = run 8080 $ simpleCors $ serve api (mock api Proxy)
