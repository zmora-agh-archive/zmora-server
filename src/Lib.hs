{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeOperators       #-}

module Lib
    ( startApp
    ) where

import Network.Wai
import Network.Wai.Handler.Warp

import Servant

import Models
import MockAPI

type API =  "users"    :> Get '[JSON] [User]
       :<|> "contests" :> Capture "id" Integer :> Get '[JSON] Contest
       :<|> "contests" :> Get '[JSON] [Contest]

api :: Proxy API
api = Proxy

startApp :: IO ()
startApp = run 8080 $ serve api (mock api)
