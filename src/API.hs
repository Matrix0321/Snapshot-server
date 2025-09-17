{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE OverloadedStrings #-}

module API
  ( API
  , api
  , SnapshotAPI
  , FullAPI
  , snapshotApi
  , fullApi
  , SparqlInput(..)
  , SnapshotCreated(..)
  ) where

import           Servant
import           GHC.Generics               (Generic)
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Text                  (Text)
import           Data.OpenApi               (ToSchema, OpenApi)

import qualified Models                     as Models
import           Types                      (Page, Limit, SortBy, SortOrder)


data SparqlInput = SparqlInput
  { snapshotName :: Text
  , sortSymbols  :: [Text]
  , relations    :: [Models.RelationInput]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

newtype SnapshotCreated = SnapshotCreated
  { snapshotId :: Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)


type SnapshotAPI =
       "createSnapshotFromQuery" :> ReqBody '[JSON] SparqlInput
                                  :> Post '[JSON] SnapshotCreated
  :<|> "snapshots" :> Capture "name" Text
                   :> Get   '[JSON] Models.Snapshot
  :<|> "snapshots"
         :> QueryParam "page"   Page
         :> QueryParam "limit"  Limit
         :> QueryParam "sortBy" SortBy
         :> QueryParam "order"  SortOrder
         :> QueryParam "q"      Text
         :> Get   '[JSON] [Models.Snapshot]
  :<|> "snapshots" :> Capture "name" Text
                   :> "signature"
                   :> Get   '[JSON] Models.Signature
  :<|> "snapshots" :> Verb 'DELETE 204 '[JSON] NoContent
  :<|> "snapshots" :> Capture "name" Text
                   :> Verb 'DELETE 204 '[JSON] NoContent


type FullAPI = SnapshotAPI
          :<|> "openapi.json" :> Get '[JSON] OpenApi

snapshotApi :: Proxy SnapshotAPI
snapshotApi = Proxy

fullApi :: Proxy FullAPI
fullApi = Proxy

type API = FullAPI

api :: Proxy API
api = Proxy
