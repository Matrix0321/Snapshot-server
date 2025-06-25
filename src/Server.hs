{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Server (server, makeApp) where

import           Control.Monad.IO.Class        (liftIO)
import           Data.Maybe                    (fromMaybe)
import qualified Data.Text                     as T
import           Data.List                     (sortBy)
import           Data.Ord                      (comparing)
import qualified Data.ByteString.Lazy          as BL
import           Database.SQLite.Simple        (Connection)
import           Servant
import           Servant.OpenApi               (toOpenApi)
import           Data.Aeson                    (encode)

import qualified API
import qualified Models                        as Models
import qualified DB                            as DB
import qualified SnapshotGenerator
import qualified Error
import           Types
import           Validation                    (validateSparqlInput)


createSnapshotHandler :: Connection -> API.SparqlInput -> Handler API.SnapshotCreated
createSnapshotHandler conn inp = do
  validateSparqlInput inp
  eres <- liftIO $ SnapshotGenerator.generateSnapshot conn inp
  case eres of
    Left errTxt ->
      throwError err500 { errBody = Error.encodeError 500 (T.unpack errTxt) }
    Right (_, cr) -> pure cr

getSnapshotHandler :: Connection -> T.Text -> Handler Models.Snapshot
getSnapshotHandler conn name = do
  xs <- liftIO $ DB.getAllSnapshots conn
  case filter (\s -> Models.snapshotName (Models.metadata s) == name) xs of
    (s:_) -> pure s
    []    -> throwError err404 { errBody = Error.encodeError 404 "Snapshot not found" }

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
      start   = (p - 1) * l
      paged   = take l . drop start $ sorted
  pure paged

getSignatureHandler :: Connection -> T.Text -> Handler Models.Signature
getSignatureHandler conn name = do
  xs <- liftIO $ DB.getAllSnapshots conn
  case [ Models.metadata s | s <- xs, Models.snapshotName (Models.metadata s) == name ] of
    (meta:_) ->
      pure $ Models.Signature (Models.sortSymbols meta) (Models.relationSignatures meta)
    []       ->
      throwError err404 { errBody = Error.encodeError 404 "Signature not found" }

deleteAllSnapshotsHandler :: Connection -> Handler NoContent
deleteAllSnapshotsHandler conn = liftIO (DB.deleteAllSnapshots conn) >> pure NoContent

deleteSnapshotHandler :: Connection -> T.Text -> Handler NoContent
deleteSnapshotHandler conn name = do
  xs <- liftIO $ DB.getAllSnapshots conn
  let remaining = filter (\s -> Models.snapshotName (Models.metadata s) /= name) xs
  liftIO $ DB.deleteAllSnapshots conn
  liftIO $ mapM_ (DB.insertSnapshot conn) remaining
  pure NoContent

openApiHandler :: Handler BL.ByteString
openApiHandler = pure $ encode (toOpenApi API.snapshotApi)


snapshotServer :: Connection -> Server API.SnapshotAPI
snapshotServer conn =
       createSnapshotHandler     conn
  :<|> getSnapshotHandler        conn
  :<|> getSnapshotsHandler       conn
  :<|> getSignatureHandler       conn
  :<|> deleteAllSnapshotsHandler conn
  :<|> deleteSnapshotHandler     conn

server :: Connection -> Server API.API
server conn = snapshotServer conn :<|> openApiHandler

makeApp :: Connection -> Application
makeApp conn = serve API.api (server conn)
