{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server (server) where

import Control.Monad.IO.Class    (liftIO)
import qualified Data.Text       as T
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Lazy       as BL
import Database.SQLite.Simple    (Connection)
import Servant
import Servant.Server ()
import Servant.OpenApi           (toOpenApi)
import Data.Aeson                (encode)

import qualified API
import qualified Models
import qualified DB
import SnapshotGenerator         (generateSnapshot)

-- 1. 创建 Snapshot：调用 generateSnapshot
createSnapshotHandler :: Connection -> API.SparqlInput -> Handler API.SnapshotCreated
createSnapshotHandler conn inp = do
  eres <- liftIO $ generateSnapshot conn inp
  case eres of
    Left err ->
      throwError err500 { errBody = BL8.pack (T.unpack err) }
    Right (_, created) ->
      pure created

-- 2. 根据 name 查询 Snapshot
getSnapshotHandler :: Connection -> T.Text -> Handler Models.Snapshot
getSnapshotHandler conn n = do
  xs <- liftIO $ DB.getAllSnapshots conn
  case [ s | s <- xs, Models.snapshotName (Models.metadata s) == n ] of
    (s:_) -> pure s
    []    -> throwError err404 { errBody = "Snapshot not found" }

-- 3. 列出所有 Snapshots
listSnapshotsHandler :: Connection -> Handler [Models.Snapshot]
listSnapshotsHandler = liftIO . DB.getAllSnapshots

-- 4. 获取 Snapshot 的 signature
getSignatureHandler :: Connection -> T.Text -> Handler Models.Signature
getSignatureHandler conn n = do
  xs <- liftIO $ DB.getAllSnapshots conn
  case [ Models.metadata s
       | s <- xs
       , Models.snapshotName (Models.metadata s) == n
       ] of
    (meta:_) ->
      pure (Models.Signature (Models.sortSymbols meta)
                             (Models.relationSignatures meta))
    [] ->
      throwError err404 { errBody = "Signature not found" }

-- 5. 删除所有 Snapshots
deleteAllSnapshotsHandler :: Connection -> Handler NoContent
deleteAllSnapshotsHandler conn =
  liftIO (DB.deleteAllSnapshots conn) >> pure NoContent

-- 6. 删除指定 name 的 Snapshot
deleteSnapshotHandler :: Connection -> T.Text -> Handler NoContent
deleteSnapshotHandler conn n = do
  xs <- liftIO $ DB.getAllSnapshots conn
  liftIO $ do
    DB.deleteAllSnapshots conn
    mapM_ (DB.insertSnapshot conn)
          [ s | s <- xs
              , Models.snapshotName (Models.metadata s) /= n
          ]
  pure NoContent

-- 7. 返回 OpenAPI 文档
openApiHandler :: Handler BL.ByteString
openApiHandler = pure $ encode (toOpenApi API.snapshotApi)

-- 组合所有处理函数
server :: Connection -> Server API.FullAPI
server conn = snapshotServer conn :<|> openApiHandler
snapshotServer :: Connection -> Server API.SnapshotAPI
snapshotServer conn =
       createSnapshotHandler     conn
  :<|> getSnapshotHandler        conn
  :<|> listSnapshotsHandler      conn
  :<|> getSignatureHandler       conn
  :<|> deleteAllSnapshotsHandler conn
  :<|> deleteSnapshotHandler     conn   
