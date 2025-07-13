{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Server (server, makeApp) where

import Control.Monad.IO.Class      (liftIO)
import Data.Maybe                  (fromMaybe)
import Data.Text                   (Text)
import qualified Data.Text         as T
import Data.List                   (sortBy)
import Data.Ord                    (comparing)
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Lazy        as BL
import Database.SQLite.Simple      (Connection)
import Network.Wai                 (Application)
import Servant
import Servant.OpenApi             (toOpenApi)
import Data.Aeson                  (encode)

import Validation                  (validateSparqlInput)
import API                         (API, SnapshotAPI, snapshotApi, api
                                   , SparqlInput(..), SnapshotCreated(..))
import qualified Models            as M
import qualified DB                as DB
import qualified SnapshotGenerator as SG
import Types

-- 1. 创建 Snapshot
createSnapshotHandler :: Connection -> SparqlInput -> Handler SnapshotCreated
createSnapshotHandler conn inp = do
  validateSparqlInput inp
  eres <- liftIO $ SG.generateSnapshot conn inp
  case eres of
    Left err      -> throwError err500 { errBody = BL8.pack (T.unpack err) }
    Right (_, cr) -> pure cr

-- 2. 单条查询
getSnapshotHandler :: Connection -> Text -> Handler M.Snapshot
getSnapshotHandler conn name = do
  xs <- liftIO $ DB.getAllSnapshots conn
  case filter (\s -> M.snapshotName (M.metadata s) == name) xs of
    (s:_) -> pure s
    []    -> throwError err404 { errBody = "Snapshot not found" }

-- 3. 列表（分页 / 过滤 / 排序）
getSnapshotsHandler
  :: Connection
  -> Maybe Page   -> Maybe Limit
  -> Maybe SortBy -> Maybe SortOrder
  -> Maybe Text   -> Handler [M.Snapshot]
getSnapshotsHandler conn mP mL mS mO mQ = do
  allSnaps <- liftIO $ DB.getAllSnapshots conn
  let filtered = case mQ of
        Just q  -> filter (\s -> q `T.isInfixOf` M.snapshotName (M.metadata s))
                          allSnaps
        Nothing -> allSnaps
      cmp = case mS of
        Just SortByCreatedAt -> comparing (M.createdAt . M.metadata)
        Just SortById        -> comparing M.snapshotId
        _                    -> comparing (M.createdAt . M.metadata)
      sorted = case mO of
        Just Asc -> sortBy cmp filtered
        _        -> sortBy (flip cmp) filtered
      Page p  = fromMaybe (Page 1 ) mP
      Limit l = fromMaybe (Limit 20) mL
      start   = (p - 1) * l
      paged   = take l . drop start $ sorted
  pure paged

-- 4. 获取签名
getSignatureHandler :: Connection -> Text -> Handler M.Signature
getSignatureHandler conn name = do
  xs <- liftIO $ DB.getAllSnapshots conn
  case [ M.metadata s | s <- xs
                      , M.snapshotName (M.metadata s) == name ] of
    (meta:_) ->
      pure $ M.Signature
        (M.sortSymbols meta)
        (M.relationSignatures meta)
    []       -> throwError err404 { errBody = "Signature not found" }

-- 5. 删除所有
deleteAllSnapshotsHandler :: Connection -> Handler NoContent
deleteAllSnapshotsHandler conn =
  liftIO (DB.deleteAllSnapshots conn) >> pure NoContent

-- 6. 删除单条
deleteSnapshotHandler :: Connection -> Text -> Handler NoContent
deleteSnapshotHandler conn name = do
  xs <- liftIO $ DB.getAllSnapshots conn
  let remaining = filter (\s -> M.snapshotName (M.metadata s) /= name) xs
  liftIO $ DB.deleteAllSnapshots conn >> mapM_ (DB.insertSnapshot conn) remaining
  pure NoContent

-- 7. OpenAPI 文档
openApiHandler :: Handler BL.ByteString
openApiHandler = pure $ encode (toOpenApi snapshotApi)

-- 8. 组合并导出
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

-- 9. Application 供外部（测试等）直接使用
makeApp :: Connection -> Application
makeApp conn = serve api (server conn)
