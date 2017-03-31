module Main where

import Server

main :: IO ()
main = do
  putStrLn "Starting API Server"
  startApp
