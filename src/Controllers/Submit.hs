{-# LANGUAGE FlexibleInstances #-}
module Controllers.Submit where

import Utils.Controller
import Models

instance HasController (Int64 -> Int64 -> HandlerT IO [Entity Submit]) where
  resourceController contestId problemId = undefined -- TODO
instance HasController (Int64 -> Int64 -> Int64 -> HandlerT IO Submit) where
  resourceController contestId problemId submitId = undefined -- TODO
