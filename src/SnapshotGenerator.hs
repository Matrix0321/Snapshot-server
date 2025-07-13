{-# LANGUAGE OverloadedStrings #-}

-- src/SnapshotGenerator.hs
module SnapshotGenerator
  ( generateSnapshot
  ) where

import Control.Exception      (try, SomeException)
import Control.Monad          (forM)
import Data.UUID.V4           (nextRandom)
import Data.UUID              (toText)
import Data.Time              (getCurrentTime, formatTime, defaultTimeLocale)
import qualified Data.Text           as T
import qualified Data.Map.Strict     as Map
import qualified Data.ByteString.Lazy.Char8 as BL8

-- 导入 Response 及它的 responseBody 字段访问器
import Network.HTTP.Client    (Response, responseBody)

-- 从 sparql-protocol 包里导入 select
import Database.SPARQL.Protocol.Client
  ( select
  , SelectResult(..)
  , RDFTerm(..)
  )
import Database.SQLite.Simple (Connection)

import qualified DB
import qualified Models
import qualified API

-- | 从 RDFTerm 提取文本
extractText :: Maybe RDFTerm -> T.Text
extractText (Just (LiteralLang t _)) = t
extractText (Just (Literal t))        = t
extractText (Just (LiteralType t _))  = t
extractText (Just (IRI uri))          = uri
extractText (Just (Blank b))          = b
extractText _                         = T.empty

-- | 核心函数：执行 SPARQL、构造 Snapshot 并写入数据库
--   返回 Left 错误信息 或 Right (Snapshot, SnapshotCreated)
generateSnapshot
  :: Connection
  -> API.SparqlInput
  -> IO (Either T.Text (Models.Snapshot, API.SnapshotCreated))
generateSnapshot conn (API.SparqlInput name sortSyms relInputs) = do
  -- 1. 生成新 ID 和时间戳
  newId  <- toText <$> nextRandom
  nowUtc <- getCurrentTime
  let createdAt = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" nowUtc
      endpoint  = "https://query.wikidata.org/sparql"

  -- 2. 对每个 RelationInput 执行 SPARQL 查询
  relResults <- forM relInputs $ \ri -> do
    let q       = Models.riQuery ri
        relName = Models.riRelationName ri
        dom     = Models.riDomainSort ri
        rng     = Models.riRangeSort ri
        vars    = Models.riVariables ri

    -- 用 select 发起查询
    eres <- try (select endpoint (BL8.pack $ T.unpack q))
             :: IO (Either SomeException (Response SelectResult))

    case eres of
      Left ex ->
        pure . Left $
          "SPARQL 查询失败 (" <> relName <> "): " <> T.pack (show ex)
      Right resp ->
        let SelectResult rows = responseBody resp
            facts = [ Map.fromList
                      [ (v, extractText (Map.lookup v r))
                      | v <- vars
                      ]
                    | r <- rows
                    ]
        in pure $ Right (relName, facts, dom, rng, vars)

  -- 3. 如果有任何查询失败，则返回错误
  case sequence relResults of
    Left errMsg ->
      pure $ Left errMsg

    Right triples -> do
      -- 4. 构造 SnapshotMetadata 与 Snapshot
      let relationsMap = Map.fromList
            [ (n, fs) | (n, fs, _, _, _) <- triples ]
          relSigs = [ Models.RelationSignature n vs d r
                    | (n, _, d, r, vs) <- triples ]
          meta = Models.SnapshotMetadata
            { Models.snapshotName       = name
            , Models.author             = "System"
            , Models.createdAt          = createdAt
            , Models.description        = T.intercalate "; "
                                          [ n | (n, _, _, _, _) <- triples ]
            , Models.signatureId        = "wikidata-auto"
            , Models.sortSymbols        = sortSyms
            , Models.relationSignatures = relSigs
            }
          snap = Models.Snapshot newId meta relationsMap

      -- 5. 插入数据库
      eresIns <- try (DB.insertSnapshot conn snap)
                 :: IO (Either SomeException ())

      case eresIns of
        Left ex ->
          pure . Left $
            "数据库插入失败: " <> T.pack (show ex)

        Right () ->
          pure $ Right (snap, API.SnapshotCreated newId)
