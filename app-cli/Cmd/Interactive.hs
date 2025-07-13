{-# LANGUAGE OverloadedStrings #-}

module Cmd.Interactive (run) where

import Prelude
import System.IO    (hFlush, stdout)
import qualified Data.Text as T
import Data.Aeson   (eitherDecode)
import Network.HTTP.Simple
    ( httpLBS
    , parseRequest
    , setRequestMethod
    , setRequestHeader
    , setRequestBodyJSON
    , getResponseBody
    , getResponseStatusCode
    )

import qualified API    as APIType
import Models (RelationInput(..), Snapshot(..))

-- | Entry point, called from Main.hs
run :: IO ()
run = interactiveMain

-- | Start interactive loop
interactiveMain :: IO ()
interactiveMain = do
  putStrLn "🚀 Welcome to Snapshot CLI Interactive Mode."
  loop

-- | Prompt helper
ask :: String -> IO String
ask prompt = do
  putStr (prompt ++ ": ")
  hFlush stdout
  getLine

-- | Main loop: list, create, or quit
loop :: IO ()
loop = do
  cmd <- ask "Choose (l)ist snapshots, (c)reate snapshot, (q)uit"
  case cmd of
    "l" -> listLoop >> loop
    "c" -> createLoop >> loop
    _   -> putStrLn "Goodbye!"

-- | List snapshots via GET /snapshots
listLoop :: IO ()
listLoop = do
  resp <- httpLBS "GET http://localhost:8080/snapshots"
  let raw = getResponseBody resp
  case eitherDecode raw of
    Left err   -> putStrLn $ "❌ Error listing snapshots: " ++ err
    Right snaps -> do
      putStrLn "Snapshots:"
      mapM_ print (snaps :: [Snapshot])

-- | Create a snapshot via POST /snapshots
createLoop :: IO ()
createLoop = do
  name  <- ask "Snapshot name"
  sorts <- ask "Sort symbols (comma-separated)"
  relN  <- ask "Relation name"
  vars  <- ask "Variables (comma-separated, in order)"
  dom   <- ask "Domain sort"
  rng   <- ask "Range sort"
  query <- ask "SPARQL query"

  let sortSyms = map T.strip . T.splitOn "," . T.pack $ sorts
      varList  = map T.strip . T.splitOn "," . T.pack $ vars
      relInput = RelationInput
                   { riRelationName = T.pack relN
                   , riVariables    = varList
                   , riQuery        = T.pack query
                   , riDomainSort   = T.pack dom
                   , riRangeSort    = T.pack rng
                   }
      sparqlIn = APIType.SparqlInput
                    { APIType.snapshotName = T.pack name
                    , APIType.sortSymbols  = sortSyms
                    , APIType.relations    = [relInput]
                    }

  initReq <- parseRequest "POST http://localhost:8080/snapshots"
  let req = setRequestMethod "POST"
          $ setRequestHeader "Content-Type" ["application/json"]
          $ setRequestBodyJSON sparqlIn initReq

  resp <- httpLBS req
  let status = getResponseStatusCode resp
      raw    = getResponseBody resp
  putStrLn $ "HTTP status: " ++ show status
  putStrLn $ "Raw response body: " ++ show raw
  case eitherDecode raw of
    Left err -> putStrLn $ "❌ JSON parse error: " ++ err
    Right (APIType.SnapshotCreated newId) ->
      putStrLn $ "✅ Created snapshot, new ID = " ++ T.unpack newId

