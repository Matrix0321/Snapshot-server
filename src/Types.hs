{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Servant       (FromHttpApiData, parseUrlPiece)
import Data.Text     (Text)
import Data.OpenApi  (ToParamSchema)
import GHC.Generics  (Generic)

-- page ----------
newtype Page = Page Int
  deriving (Eq, Show, Generic)

instance ToParamSchema Page
instance FromHttpApiData Page where
  parseUrlPiece t =
    case parseUrlPiece t of
      Left e -> Left e
      Right n | n > 0     -> Right (Page n)
              | otherwise -> Left "`page` must be a positive integer"

-- limit ---------
newtype Limit = Limit Int
  deriving (Eq, Show, Generic)

instance ToParamSchema Limit
instance FromHttpApiData Limit where
  parseUrlPiece t =
    case parseUrlPiece t of
      Left e -> Left e
      Right n | n > 0     -> Right (Limit n)
              | otherwise -> Left "`limit` must be a positive integer"

-- sortBy --------
data SortBy = SortByCreatedAt | SortById
  deriving (Eq, Show, Generic)

instance ToParamSchema SortBy
instance FromHttpApiData SortBy where
  parseUrlPiece "created_at" = Right SortByCreatedAt
  parseUrlPiece "id"         = Right SortById
  parseUrlPiece _            = Left "invalid `sortBy` field"

-- order ---------
data SortOrder = Asc | Desc
  deriving (Eq, Show, Generic)

instance ToParamSchema SortOrder
instance FromHttpApiData SortOrder where
  parseUrlPiece "ASC"  = Right Asc
  parseUrlPiece "DESC" = Right Desc
  parseUrlPiece _      = Left "`order` must be `ASC` or `DESC`"
