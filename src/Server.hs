{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Server ( startApp ) where

import Network.Wai
import Network.Wai.Middleware.Cors
import Network.Wai.Handler.Warp
import Network.Wai.Logger

import Database.Persist.Postgresql

import Servant
import Servant.Mock

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (ExceptT, throwE)
import           Diener
import qualified Diener.Logger          as Logger

import Api
import Fake
import Models
import Types
import Controller

server :: LogEnv HandlerEnv -> Server API
server env = enter dienerToEither controller
  where
    dienerToEither :: HandlerT IO :~> ExceptT ServantErr IO
    dienerToEither = Nat $ \ar ->
      liftIO (runDienerT env ar) >>= \case
        Left err -> throwE (dienerErrToServantErr err)
        Right a  -> return a

dienerErrToServantErr :: AppError -> ServantErr
dienerErrToServantErr ErrNotFound = err404 { errBody = "The requested resource could not be found."}
dienerErrToServantErr _           = err500 { errBody = "Internal server error."}

startApp :: IO ()
startApp = do
  let logSettings = Logger.Settings { Logger.filePath = "server.log"
                                    , Logger.logLevel = LevelDebug
                                    , Logger.noConsoleLogging = False
                                    }
  let dbSettings = "host=localhost port=5432 user=zmora dbname=zmora password=szatan"

  withStdoutLogger $ \apacheLogger ->
    withLogger logSettings $ \logger ->
      withPostgresqlPool dbSettings 1 $ \pool -> do
        runSqlPool (runMigration migrateAll) pool
        let settings = setPort 8080 $ setLogger apacheLogger defaultSettings
        let env = LogEnv logger $ HandlerEnv pool
        liftIO $ runSettings settings $ simpleCors $ serve api (server env)
