{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Options.Applicative
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.HTTP.Simple
import Control.Exception (catch, SomeException)
import Data.Aeson (eitherDecode)
import Control.Monad (forM_)
import Control.Concurrent (threadDelay)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

import qualified API
import qualified Cmd.Interactive as Interactive
import qualified Cmd.ServeUI     as ServeUI

-- | All supported subcommands
data Command
  = List
  | ShowSnapshot T.Text
  | DeleteSnapshot T.Text
  | Batch FilePath
  | InteractiveMode
  | ServeUIMode { port :: Int }

-- | Parser for the top-level subcommands
commandParser :: Parser Command
commandParser = hsubparser
  (  command "list"
       (info (pure List)
         (progDesc "List all snapshots"))
  <> command "show"
       (info (ShowSnapshot <$> argument str (metavar "NAME"))
         (progDesc "Show one snapshot by NAME"))
  <> command "delete"
       (info (DeleteSnapshot <$> argument str (metavar "NAME"))
         (progDesc "Delete one snapshot by NAME"))
  <> command "batch"
       (info (Batch <$> argument str (metavar "CONFIG.json"))
         (progDesc "Batch generate from JSON config file"))
  <> command "interactive"
       (info (pure InteractiveMode)
         (progDesc "Enter terminal interactive mode"))
  <> command "serve-ui"
       (info (ServeUIMode <$> portParser)
         (progDesc "Start browser-based UI (default port 8081)"))
  )

-- | Parser for the port flag of serve-ui
portParser :: Parser Int
portParser = option auto
  ( long "port"
 <> short 'p'
 <> metavar "PORT"
 <> value 8081
 <> showDefault
 <> help "Port for the UI server"
  )

-- | Combine parser with helper
opts :: ParserInfo Command
opts = info (helper <*> commandParser)
  ( fullDesc
 <> header "snapshot-server-cli - CLI for snapshot-server"
  )

main :: IO ()
main = execParser opts >>= run

-- | Dispatch to each subcommand
run :: Command -> IO ()
-- ===== Existing commands =====
run List = do
  resp <- httpLBS "GET http://localhost:8080/snapshots"
  BL.putStrLn $ getResponseBody resp

run (ShowSnapshot name) = do
  let url = "http://localhost:8080/snapshots/" ++ T.unpack name
  resp <- httpLBS (parseRequest_ url)
  BL.putStrLn $ getResponseBody resp

run (DeleteSnapshot name) = do
  let url = "http://localhost:8080/snapshots/" ++ T.unpack name
  req0 <- parseRequest url
  let req = setRequestMethod "DELETE" req0
  resp <- httpLBS req
  print $ getResponseStatus resp

run (Batch cfgPath) = do
  raw <- BL.readFile cfgPath
  let decoded = eitherDecode raw :: Either String [API.SparqlInput]
  case decoded of
    Left err -> putStrLn $ "Failed to parse config: " ++ err
    Right inputs -> do
      let logFile = "snapshot_report.csv"
      writeFile logFile "snapshotName,status,newId,errorMessage,timestamp\n"
      forM_ inputs $ \inp -> do
        now2   <- getCurrentTime
        initReq <- parseRequest "POST http://localhost:8080/snapshots"
        let req = setRequestMethod "POST"
                $ setRequestHeader "Content-Type" ["application/json"]
                $ setRequestBodyJSON inp initReq
            httpAction = Right . getResponseBody <$> httpLBS req
              :: IO (Either String BL.ByteString)
        eres <- catch httpAction $ \(e :: SomeException) ->
                  pure (Left (show e))
        let ts   = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now2
            name = API.snapshotName inp
        case eres of
          Left ex -> appendLog logFile name "FAILED" "" (T.pack ex) ts
          Right b -> case eitherDecode b of
            Left je   -> appendLog logFile name "FAILED" "" (T.pack je) ts
            Right spr -> appendLog logFile name "SUCCESS"
                                         (T.unpack (API.snapshotId spr))
                                         "" ts
        threadDelay 1000000

-- ===== New commands =====
run InteractiveMode =
  Interactive.run
run (ServeUIMode p) =
  ServeUI.run p

-- | Append log entry
appendLog
  :: FilePath -> T.Text -> String -> String -> T.Text -> String -> IO ()
appendLog fp name status newId errMsg ts =
  appendFile fp $
    T.unpack name ++ ","
    ++ status     ++ ","
    ++ newId      ++ ","
    ++ T.unpack errMsg ++ ","
    ++ ts ++ "\n"
