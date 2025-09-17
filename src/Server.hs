{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Server (server, makeApp, app) where

import           Control.Monad.IO.Class   (liftIO)
import           Data.Maybe               (fromMaybe)
import qualified Data.Text                as T
import           Data.List                (sortBy)
import           Data.Ord                 (comparing)

import           Servant
import           Servant.OpenApi          (toOpenApi)
import           Data.OpenApi             (OpenApi)
import           Database.SQLite.Simple   (Connection)

import           API
import qualified Models                   as Models
import qualified DB                       as DB
import qualified SnapshotGenerator        as SG
import           Error
import           Types
import           Validation               (validateSparqlInput)

-- POST /createSnapshotFromQuery
createSnapshotHandler :: Connection -> SparqlInput -> Handler SnapshotCreated
createSnapshotHandler conn inp = do
  validateSparqlInput inp
  eres <- liftIO $ SG.generateSnapshot conn inp
  case eres of
    Left errTxt      -> throwError err500 { errBody = encodeError 500 (T.unpack errTxt) }
    Right (_, resp ) -> pure resp

-- GET /snapshots/:name
getSnapshotHandler :: Connection -> T.Text -> Handler Models.Snapshot
getSnapshotHandler conn name = do
  allSnaps <- liftIO $ DB.getAllSnapshots conn
  case filter (\s -> Models.snapshotName (Models.metadata s) == name) allSnaps of
    (s:_) -> pure s
    []    -> throwError err404 { errBody = encodeError 404 "Snapshot not found" }

-- GET /snapshots?page&limit&sortBy&order&q
getSnapshotsHandler
  :: Connection
  -> Maybe Page   -> Maybe Limit
  -> Maybe SortBy -> Maybe SortOrder
  -> Maybe T.Text -> Handler [Models.Snapshot]
getSnapshotsHandler conn mP mL mS mO mQ = do
  allSnaps <- liftIO $ DB.getAllSnapshots conn
  let filtered = case mQ of
        Just q  -> filter (\s -> q `T.isInfixOf` Models.snapshotName (Models.metadata s)) allSnaps
        Nothing -> allSnaps

      cmp = case mS of
        Just SortByCreatedAt -> comparing (Models.createdAt . Models.metadata)
        Just SortById        -> comparing Models.snapshotId
        _                    -> comparing (Models.createdAt . Models.metadata)

      sorted = case mO of
        Just Asc -> sortBy cmp filtered
        _        -> sortBy (flip cmp) filtered

      Page p  = fromMaybe (Page 1 ) mP
      Limit l = fromMaybe (Limit 20) mL
      start   = max 0 ((p - 1) * l)
      paged   = take l . drop start $ sorted

  pure paged

-- GET /snapshots/:name/signature
getSignatureHandler :: Connection -> T.Text -> Handler Models.Signature
getSignatureHandler conn name = do
  allSnaps <- liftIO $ DB.getAllSnapshots conn
  case [ Models.metadata s | s <- allSnaps, Models.snapshotName (Models.metadata s) == name ] of
    (meta:_) -> pure $ Models.Signature (Models.sortSymbols meta) (Models.relationSignatures meta)
    []       -> throwError err404 { errBody = encodeError 404 "Signature not found" }

-- DELETE /snapshots
deleteAllSnapshotsHandler :: Connection -> Handler NoContent
deleteAllSnapshotsHandler conn = liftIO (DB.deleteAllSnapshots conn) >> pure NoContent

-- DELETE /snapshots/:name
deleteSnapshotHandler :: Connection -> T.Text -> Handler NoContent
deleteSnapshotHandler conn name = do
  xs <- liftIO $ DB.getAllSnapshots conn
  let remaining = filter (\s -> Models.snapshotName (Models.metadata s) /= name) xs
  liftIO $ DB.deleteAllSnapshots conn
  liftIO $ mapM_ (DB.insertSnapshot conn) remaining
  pure NoContent

-- GET /openapi.json
openApiHandler :: Handler OpenApi
openApiHandler = pure (toOpenApi snapshotApi)

-- Business routes only
snapshotServer :: Connection -> Server SnapshotAPI
snapshotServer conn =
       createSnapshotHandler     conn
  :<|> getSnapshotHandler        conn
  :<|> getSnapshotsHandler       conn
  :<|> getSignatureHandler       conn
  :<|> deleteAllSnapshotsHandler conn
  :<|> deleteSnapshotHandler     conn


server :: Connection -> Server API
server conn = snapshotServer conn :<|> openApiHandler

makeApp :: Connection -> Application
makeApp conn = serve api (server conn)

app :: Connection -> Application
app = makeApp
