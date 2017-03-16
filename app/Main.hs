module Main where

import Api

main :: IO ()
main = do
  putStrLn "Starting API Server"
  startApp
