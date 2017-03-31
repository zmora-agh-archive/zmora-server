{-# LANGUAGE FlexibleInstances #-}
module Controllers.Time where

import Control.Monad.IO.Class

import Utils.Controller
import Models

instance HasController (HandlerT IO CurrentTime) where
  resourceController = liftIO getCurrentTime
