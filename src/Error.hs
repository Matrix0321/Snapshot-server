{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Error
  ( ErrorResp(..)
  , encodeError
  ) where

import           Data.Aeson           (ToJSON, encode)
import qualified Data.ByteString.Lazy as BL
import           GHC.Generics         (Generic)

data ErrorResp = ErrorResp
  { errCode    :: Int
  , errMessage :: String
  } deriving (Show, Eq, Generic)

instance ToJSON ErrorResp

encodeError :: Int -> String -> BL.ByteString
encodeError c msg = encode (ErrorResp c msg)
