{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
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

import Servant
import Servant.API.ContentTypes ()
import GHC.Generics             (Generic)
import Data.Aeson               (FromJSON, ToJSON)
import Data.Text                (Text)
import qualified Data.ByteString.Lazy as BL
import qualified Models         as M
import Data.OpenApi             (ToSchema)


data SparqlInput = SparqlInput
  { snapshotName :: Text
  , sortSymbols  :: [Text]
  , relations    :: [M.RelationInput]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

newtype SnapshotCreated = SnapshotCreated
  { snapshotId :: Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)


type SnapshotAPI =
       "snapshots" :> ReqBody '[JSON] SparqlInput :> Post '[JSON] SnapshotCreated
  :<|> "snapshots" :> Capture "name" Text :> Get '[JSON] M.Snapshot
  :<|> "snapshots" :> Get '[JSON] [M.Snapshot]
  :<|> "snapshots" :> Capture "name" Text :> "signature" :> Get '[JSON] M.Signature
  :<|> "snapshots" :> Verb 'DELETE 204 '[JSON] NoContent
  :<|> "snapshots" :> Capture "name" Text :> Verb 'DELETE 204 '[JSON] NoContent


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
