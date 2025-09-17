{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           GHC.Generics               (Generic)
import           Data.Aeson
  ( FromJSON
  , eitherDecode
  , encode
  , Value
  , object
  , (.=)
  )
import           Data.Aeson.Encode.Pretty   (encodePretty')
import           Data.Aeson.Encode.Pretty   as Pretty (Config (..), defConfig, Indent(..))
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Network.HTTP.Client        ( defaultManagerSettings
                                            , newManager
                                            , parseRequest
                                            , responseStatus
                                            , responseBody
                                            , httpLbs
                                            , method
                                            , requestHeaders
                                            , requestBody
                                            , RequestBody (..)
                                            )
import           Network.HTTP.Types.Status  (statusCode)
import           System.Environment         (getArgs)
import           System.Exit                (exitFailure)


data SnapshotMeta = SnapshotMeta
  { snapshotName :: T.Text
  } deriving (Show, Generic)
instance FromJSON SnapshotMeta

data SnapshotForList = SnapshotForList
  { metadata :: SnapshotMeta
  } deriving (Show, Generic)
instance FromJSON SnapshotForList

usage :: IO ()
usage = do
  putStrLn "snapshot-server-cli usage:"
  putStrLn "  stack run snapshot-server-cli -- list"
  putStrLn "  stack run snapshot-server-cli -- get <name>"
  putStrLn "  stack run snapshot-server-cli -- create <name>"
  putStrLn "  stack run snapshot-server-cli -- delete-all"
  putStrLn "  stack run snapshot-server-cli -- delete <name>"
  putStrLn ""
  putStrLn "Server endpoint defaults to http://localhost:8080/"

prettyCfg :: Pretty.Config
prettyCfg = Pretty.defConfig { confIndent = Spaces 2 }

main :: IO ()
main = do
  args <- getArgs
  mgr  <- newManager defaultManagerSettings
  case args of
    ["list"] -> do
      req0 <- parseRequest "http://localhost:8080/snapshots?limit=10000"
      resp <- httpLbs req0 mgr
      let body = responseBody resp
      case eitherDecode body :: Either String [SnapshotForList] of
        Left e -> do
          putStrLn $ "HTTP " ++ show (statusCode (responseStatus resp))
          putStrLn $ "[decode-error] " ++ e
          BL.putStr body
          exitFailure
        Right xs -> mapM_ (TIO.putStrLn . snapshotName . metadata) xs

    ["get", name] -> do
      req0 <- parseRequest ("http://localhost:8080/snapshots/" ++ name)
      resp <- httpLbs req0 mgr
      let body = responseBody resp
      case eitherDecode body :: Either String Value of
        Left _  -> BL.putStr body 
        Right v -> BL.putStr (encodePretty' prettyCfg v)

    ["create", name] -> do
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
      req0 <- parseRequest "http://localhost:8080/createSnapshotFromQuery"
      let req = req0 { method = "POST"
                     , requestHeaders = [("Content-Type", "application/json")]
                     , requestBody    = RequestBodyLBS (encode payload)
                     }
      resp <- httpLbs req mgr
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
