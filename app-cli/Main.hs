{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson                (object, (.=), encode)
import qualified Data.ByteString.Lazy      as BL
import qualified Data.Text                 as T
import           Network.HTTP.Client       (defaultManagerSettings, newManager, parseRequest, responseStatus, responseBody, httpLbs, method, requestHeaders, requestBody, RequestBody (..))
import           Network.HTTP.Types.Status (statusCode)
import           System.Environment        (getArgs)
import           System.Exit               (exitFailure)

usage :: IO ()
usage = do
  putStrLn "snapshot-server-cli usage:"
  putStrLn "  snapshot-server-cli list"
  putStrLn "  snapshot-server-cli get <name>"
  putStrLn "  snapshot-server-cli create <name>"
  putStrLn "  snapshot-server-cli delete-all"
  putStrLn "  snapshot-server-cli delete <name>"
  putStrLn ""
  putStrLn "Server endpoint is assumed at http://localhost:8080/"

main :: IO ()
main = do
  args <- getArgs
  mgr  <- newManager defaultManagerSettings
  case args of
    ["list"] -> do
      req0 <- parseRequest "http://localhost:8080/snapshots"
      resp <- httpLbs req0 mgr
      putStrLn $ "HTTP " ++ show (statusCode (responseStatus resp))
      BL.putStr (responseBody resp)

    ["get", name] -> do
      req0 <- parseRequest ("http://localhost:8080/snapshots/" ++ name)
      resp <- httpLbs req0 mgr
      putStrLn $ "HTTP " ++ show (statusCode (responseStatus resp))
      BL.putStr (responseBody resp)

    ["create", name] -> do
      -- Minimal demo payload (adjust as needed)
      let payload = object
            [ "snapshotName" .= T.pack name
            , "sortSymbols"  .= [T.pack "Country", T.pack "City"]
            , "relations" .=
                [ object
                    [ "riRelationName" .= T.pack "country_capital"
                    , "riVariables"    .= [T.pack "countryLabel", T.pack "capitalLabel"]
                    , "riQuery"        .= T.pack
                        "SELECT ?countryLabel ?capitalLabel WHERE { ?country wdt:P31 wd:Q6256; wdt:P36 ?capital. SERVICE wikibase:label { bd:serviceParam wikibase:language \"en\". } } LIMIT 10"
                    , "riDomainSort"   .= T.pack "Country"
                    , "riRangeSort"    .= T.pack "City"
                    ]
                ]
            ]
      req0 <- parseRequest "http://localhost:8080/snapshots"
      let req = req0 { method = "POST"
                     , requestHeaders = [("Content-Type", "application/json")]
                     , requestBody    = RequestBodyLBS (encode payload)
                     }
      resp <- httpLbs req mgr
      putStrLn $ "HTTP " ++ show (statusCode (responseStatus resp))
      BL.putStr (responseBody resp)

    ["delete-all"] -> do
      req0 <- parseRequest "http://localhost:8080/snapshots"
      let req = req0 { method = "DELETE" }
      resp <- httpLbs req mgr
      putStrLn $ "HTTP " ++ show (statusCode (responseStatus resp))

    ["delete", name] -> do
      req0 <- parseRequest ("http://localhost:8080/snapshots/" ++ name)
      let req = req0 { method = "DELETE" }
      resp <- httpLbs req mgr
      putStrLn $ "HTTP " ++ show (statusCode (responseStatus resp))

    _ -> usage >> exitFailure
