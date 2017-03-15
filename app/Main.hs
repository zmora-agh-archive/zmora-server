module Main where

import Lib

main :: IO ()
main = do
  putStrLn "Starting API Server"
  startApp
