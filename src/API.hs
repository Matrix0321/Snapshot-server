{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module API
  ( API
  , api 
  , SnapshotAPI          -- 业务端点
  , FullAPI              -- = SnapshotAPI + openapi.json
  , snapshotApi          -- Proxy SnapshotAPI
  , fullApi              -- Proxy FullAPI
  , SparqlInput(..)
  , SnapshotCreated(..)
  ) where

import Servant
import Servant.API.ContentTypes ()
import GHC.Generics             (Generic)
import Data.Aeson               (FromJSON, ToJSON)
import Data.Text                (Text)
import qualified Data.ByteString.Lazy as BL
import qualified Models         as M
import Data.OpenApi             (ToSchema)

-- ====== 请求/响应数据 ======
data SparqlInput = SparqlInput
  { snapshotName :: Text
  , sortSymbols  :: [Text]
  , relations    :: [M.RelationInput]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

newtype SnapshotCreated = SnapshotCreated
  { snapshotId :: Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

-- ====== 业务端点 ======
type SnapshotAPI =
       "snapshots" :> ReqBody '[JSON] SparqlInput :> Post '[JSON] SnapshotCreated
  :<|> "snapshots" :> Capture "name" Text :> Get '[JSON] M.Snapshot
  :<|> "snapshots" :> Get '[JSON] [M.Snapshot]
  :<|> "snapshots" :> Capture "name" Text :> "signature" :> Get '[JSON] M.Signature
  :<|> "snapshots" :> Verb 'DELETE 204 '[JSON] NoContent
  :<|> "snapshots" :> Capture "name" Text :> Verb 'DELETE 204 '[JSON] NoContent

-- ====== 文档端点 ======
type FullAPI = SnapshotAPI
          :<|> "openapi.json" :> Get '[OctetStream] BL.ByteString

-- Proxy helpers
snapshotApi :: Proxy SnapshotAPI
snapshotApi = Proxy

fullApi :: Proxy FullAPI
fullApi = Proxy

type API = FullAPI

api :: Proxy API
api = Proxy