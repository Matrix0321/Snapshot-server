{-# LANGUAGE DeriveGeneric #-}
module Error
  ( ErrorResp(..)
  , encodeError       -- <<< 供 Validation / Server 使用
  ) where

import           Data.Aeson                 (ToJSON, encode)
import qualified Data.ByteString.Lazy       as BL         -- ★ 新增
import           GHC.Generics               (Generic)

-- 统一 JSON 结构
data ErrorResp = ErrorResp
  { errCode    :: Int
  , errMessage :: String
  } deriving (Generic)

instance ToJSON ErrorResp

-- | 把 (code,msg) 编成 ByteString，直接丢给 errBody
encodeError :: Int -> String -> BL.ByteString   -- ★ 之前缺少 BL
encodeError c msg = encode (ErrorResp c msg)
