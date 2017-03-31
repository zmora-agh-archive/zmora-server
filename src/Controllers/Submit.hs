{-# LANGUAGE FlexibleInstances #-}
module Controllers.Submit where

import Utils.Controller
import Models

instance HasController (CurrentUser -> Int64 -> Int64 -> HandlerT IO [Entity Submit]) where
  resourceController _ contestId problemId = undefined -- TODO

instance HasController (CurrentUser -> Int64 -> Int64 -> Int64 -> HandlerT IO Submit) where
  resourceController _ contestId problemId submitId = undefined -- TODO
