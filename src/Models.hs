{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module Models
  ( RelationInput(..)
  , RelationSignature(..)
  , Signature(..)
  , SnapshotMetadata(..)
  , Snapshot(..)
  ) where

import GHC.Generics       (Generic)
import Data.Aeson         (FromJSON, ToJSON)
import Data.OpenApi       (ToSchema)          
import Data.Text          (Text)
import qualified Data.Map.Strict as Map

-- 请求中用到
data RelationInput = RelationInput
  { riRelationName :: Text
  , riVariables    :: [Text]
  , riQuery        :: Text
  , riDomainSort   :: Text
  , riRangeSort    :: Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema) 

-- 部分响应结构
data RelationSignature = RelationSignature
  { rsRelationName :: Text
  , rsVariables    :: [Text]
  , rsDomainSort   :: Text
  , rsRangeSort    :: Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)  

data Signature = Signature
  { sorts         :: [Text]
  , relSignatures :: [RelationSignature]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)  

data SnapshotMetadata = SnapshotMetadata
  { snapshotName       :: Text
  , author             :: Text
  , createdAt          :: Text
  , description        :: Text
  , signatureId        :: Text
  , sortSymbols        :: [Text]
  , relationSignatures :: [RelationSignature]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)  

data Snapshot = Snapshot
  { snapshotId :: Text
  , metadata   :: SnapshotMetadata
  , relations  :: Map.Map Text [Map.Map Text Text]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)  
