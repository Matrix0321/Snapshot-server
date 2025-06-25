{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Wai.Handler.Warp (run)
import Servant.Server           (serve)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static        (staticPolicy, addBase)

import API                        (api)
import DB                         (initDB)
import Server                     (server)

main :: IO ()
main = do
  putStrLn "Opening SQLite database..."
  conn <- initDB

  putStrLn "Starting server on http://localhost:8080"
  run 8080 $ logStdoutDev $ staticPolicy (addBase "static") $ serve api (server conn)
