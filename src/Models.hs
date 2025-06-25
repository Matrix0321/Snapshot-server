{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE OverloadedStrings #-}

module Models
  ( Sort
  , RelationName
  , Variable
  , RelationInput(..)
  , RelationSignature(..)
  , Signature(..)
  , SnapshotId(..)
  , SnapshotMetadata(..)
  , Snapshot(..)
  ) where

import           GHC.Generics         (Generic)
import           Data.Aeson           (FromJSON, ToJSON)
import           Data.OpenApi         (ToSchema)
import           Data.Text            (Text)
import qualified Data.Map.Strict      as Map

type Sort        = Text
type RelationName = Text
type Variable    = Text

newtype SnapshotId = SnapshotId { unSnapshotId :: Text }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON, ToSchema)

data RelationInput = RelationInput
  { riRelationName :: RelationName
  , riVariables    :: [Variable]
  , riQuery        :: Text
  , riDomainSort   :: Sort
  , riRangeSort    :: Sort
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

data RelationSignature = RelationSignature
  { rsRelationName :: RelationName
  , rsVariables    :: [Variable]
  , rsDomainSort   :: Sort
  , rsRangeSort    :: Sort
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

data Signature = Signature
  { sorts         :: [Sort]
  , relSignatures :: [RelationSignature]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

data SnapshotMetadata = SnapshotMetadata
  { snapshotName       :: Text
  , author             :: Text
  , createdAt          :: Text
  , description        :: Text
  , signatureId        :: Text
  , sortSymbols        :: [Sort]
  , relationSignatures :: [RelationSignature]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

data Snapshot = Snapshot
  { snapshotId :: SnapshotId
  , metadata   :: SnapshotMetadata
  , relations  :: Map.Map RelationName [Map.Map Variable Text]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)
