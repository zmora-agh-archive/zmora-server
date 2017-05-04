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
import Servant.Auth.Server

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (ExceptT, throwE)
import           Control.Natural
import           Diener
import qualified Diener.Logger          as Logger

import Api
import Models
import Types
import Controller

server :: LogEnv HandlerEnv -> Server API
server env = enter dienerToEither controller
  where
    dienerToEither :: HandlerT IO :~> Handler
    dienerToEither = NT $ \ar ->
      liftIO (runDienerT env ar) >>= \case
        Left err -> Handler $ throwE (appErrToServantErr err)
        Right a  -> Handler $ return a

appErrToServantErr :: AppError -> ServantErr
appErrToServantErr = \case
  ErrNotFound     -> err404 { errBody = "The requested resource could not be found." }
  ErrUnauthorized -> err401 { errBody = "Please authenticate in order to continue." }
  ErrForbidden    -> err403 { errBody = "You are not authorized to access requested resource." }
  _               -> err500 { errBody = "Internal server error." }


startApp :: IO ()
startApp = do
  let logSettings = Logger.Settings { Logger.filePath = "server.log"
                                    , Logger.logLevel = LevelDebug
                                    , Logger.noConsoleLogging = False
                                    }
  let dbSettings = "host=localhost port=5432 user=zmora dbname=zmora password=szatan"

  -- TODO Make this persist, so every restart is not regenerating the key
  jwtKey <- generateKey

  let jwtSettings = defaultJWTSettings jwtKey

  let corsPolicy = CorsResourcePolicy Nothing ["POST", "GET"] ["Content-Type", "Authorization"] Nothing Nothing False True False

  withStdoutLogger $ \apacheLogger ->
    withLogger logSettings $ \logger ->
      withPostgresqlPool dbSettings 1 $ \pool -> do
        runSqlPool (runMigration migrateAll) pool
        let settings = setPort 8080 $ setLogger apacheLogger defaultSettings
        let env = LogEnv logger $ HandlerEnv pool jwtSettings
        liftIO $ runSettings settings
               $ cors (\_ -> Just corsPolicy)
               $ serveWithContext api (defaultCookieSettings :. jwtSettings :. EmptyContext)
               $ server env
