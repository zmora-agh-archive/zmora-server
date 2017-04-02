{-# LANGUAGE FlexibleInstances #-}
module Controllers.Time where

import Control.Monad.IO.Class

import Utils.Controller
import Models

instance HasController (CurrentUser -> HandlerT IO CurrentTime) where
  resourceController _ = liftIO getCurrentTime
