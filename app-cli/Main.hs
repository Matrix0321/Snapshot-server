{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Options.Applicative
import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.HTTP.Simple
import Control.Exception          (catch, SomeException)
import Data.Aeson                 (eitherDecode)
import Control.Monad              (forM_)
import Control.Concurrent         (threadDelay)
import Data.Time.Clock            (getCurrentTime)
import Data.Time.Format           (defaultTimeLocale, formatTime)

import qualified API

-- CLI 命令
data Command
  = List
  | Show T.Text
  | Delete T.Text
  | Batch FilePath

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (commands <**> helper)
      ( fullDesc
     <> progDesc "Snapshot CLI: list/show/delete or batch generate" )

commands :: Parser Command
commands = subparser
  ( command "list"
      (info (pure List) (progDesc "List all snapshots"))
 <> command "show"
      (info (Show <$> argument str (metavar "NAME"))
            (progDesc "Show one snapshot"))
 <> command "delete"
      (info (Delete <$> argument str (metavar "NAME"))
            (progDesc "Delete one snapshot"))
 <> command "batch"
      (info (Batch <$> argument str (metavar "CONFIG.json"))
            (progDesc "Batch generate from JSON config")) )

run :: Command -> IO ()
run List = do
  resp <- httpLBS "GET http://localhost:8080/snapshots"
  BL.putStrLn $ getResponseBody resp

run (Show name) = do
  let url = "http://localhost:8080/snapshots/" ++ T.unpack name
  resp <- httpLBS (parseRequest_ url)
  BL.putStrLn $ getResponseBody resp

run (Delete name) = do
  let url = "http://localhost:8080/snapshots/" ++ T.unpack name
  req0 <- parseRequest url
  let req = setRequestMethod "DELETE" req0
  resp <- httpLBS req
  print $ getResponseStatus resp

run (Batch cfgPath) = do
  raw <- BL.readFile cfgPath
  let decoded = eitherDecode raw :: Either String [API.SparqlInput]
  case decoded of
    Left err -> putStrLn $ "配置解析失败: " ++ err
    Right inputs -> do
      let logFile = "snapshot_report.csv"
      writeFile logFile "snapshotName,status,newId,errorMessage,timestamp\n"

      forM_ inputs $ \inp -> do
        now2 <- getCurrentTime

        -- 构造 POST 请求动作
        initReq <- parseRequest "POST http://localhost:8080/snapshots"
        let req = setRequestMethod "POST"
                $ setRequestHeader "Content-Type" ["application/json"]
                $ setRequestBodyJSON inp initReq
            httpAction = Right . getResponseBody <$> httpLBS req
              :: IO (Either String BL.ByteString)

        -- 捕获任意异常
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

        -- 延迟 1s 防限流
        threadDelay 1000000

-- 日志追加
appendLog
  :: FilePath -> T.Text -> String -> String -> T.Text -> String -> IO ()
appendLog fp name status newId errMsg ts =
  appendFile fp $
    T.unpack name ++ ","
    ++ status     ++ ","
    ++ newId      ++ ","
    ++ T.unpack errMsg ++ ","
    ++ ts ++ "\n"
