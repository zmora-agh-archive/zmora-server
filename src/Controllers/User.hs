{-# LANGUAGE FlexibleInstances #-}
module Controllers.User where

import Utils.Controller
import Models

instance HasController (Int64 -> HandlerT IO User) where
  resourceController = getById
