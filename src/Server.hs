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

-- 1. Generate Snapshot: generateSnapshot
createSnapshotHandler :: Connection -> API.SparqlInput -> Handler API.SnapshotCreated
createSnapshotHandler conn inp = do
  eres <- liftIO $ generateSnapshot conn inp
  case eres of
    Left err ->
      throwError err500 { errBody = BL8.pack (T.unpack err) }
    Right (_, created) ->
      pure created

-- 2. Query Snapshot by name
getSnapshotHandler :: Connection -> T.Text -> Handler Models.Snapshot
getSnapshotHandler conn n = do
  xs <- liftIO $ DB.getAllSnapshots conn
  case [ s | s <- xs, Models.snapshotName (Models.metadata s) == n ] of
    (s:_) -> pure s
    []    -> throwError err404 { errBody = "Snapshot not found" }

-- 3. List all Snapshots
listSnapshotsHandler :: Connection -> Handler [Models.Snapshot]
listSnapshotsHandler = liftIO . DB.getAllSnapshots

-- 4. Get signature of Snapshot 
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

-- 5. Delete all Snapshots
deleteAllSnapshotsHandler :: Connection -> Handler NoContent
deleteAllSnapshotsHandler conn =
  liftIO (DB.deleteAllSnapshots conn) >> pure NoContent

-- 6. Delete the snapshot with the specified name
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

-- 7. Return OpenAPI Document
openApiHandler :: Handler BL.ByteString
openApiHandler = pure $ encode (toOpenApi API.snapshotApi)

-- Combine all processing functions
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
