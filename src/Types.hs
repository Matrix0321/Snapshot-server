{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Types
  ( Page(..)
  , Limit(..)
  , SortBy(..)
  , SortOrder(..)
  ) where

import           Servant       (FromHttpApiData(..))
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Data.OpenApi  (ToParamSchema)
import           GHC.Generics  (Generic)

-- Helpers
parsePositiveInt :: Text -> Either String Int
parsePositiveInt t =
  case reads (T.unpack t) of
    [(n, "")] | n > 0 -> Right n
    _                 -> Left "must be a positive integer"

firstTxt :: Either String a -> Either Text a
firstTxt = either (Left . T.pack) Right

-- page ----------
newtype Page = Page Int
  deriving (Eq, Show, Generic)

instance ToParamSchema Page
instance FromHttpApiData Page where
  parseUrlPiece t = firstTxt (parsePositiveInt t) >>= (Right . Page)

-- limit ---------
newtype Limit = Limit Int
  deriving (Eq, Show, Generic)

instance ToParamSchema Limit
instance FromHttpApiData Limit where
  parseUrlPiece t = firstTxt (parsePositiveInt t) >>= (Right . Limit)

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
