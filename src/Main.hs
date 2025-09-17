{-# LANGUAGE OverloadedStrings #-}

module Main (main, app) where

import           Network.Wai                          (Application)
import           Network.Wai.Handler.Warp             (run)
import           Servant.Server                       (serve)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Static         (staticPolicy, addBase)
import           Database.SQLite.Simple                (Connection)

import           API    (api)
import           DB     (initDB)
import           Server (server)

app :: Connection -> Application
app conn =
  logStdoutDev
  $ staticPolicy (addBase "static")
  $ serve api (server conn)

main :: IO ()
main = do
  conn <- initDB "snapshots.db"
  putStrLn "[server] Listening on http://localhost:8080"
  putStrLn "[server] Static UI served from ./static (open in browser)"
  run 8080 (app conn)
