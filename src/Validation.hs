{-# LANGUAGE OverloadedStrings #-}
module Validation
  ( validateSparqlInput        -- :: API.SparqlInput -> Handler ()
  ) where

import           Control.Monad              (when, forM_)
import           Control.Monad.Except       (throwError)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy       as BL
import           Servant.Server             (Handler, err400, errBody)

import qualified API
import qualified Models                     as M       -- ★ 用 Models 的字段
import qualified Error

-- | 对前端 / CLI 发送的请求体做语义校验。
--   若发现问题，直接抛 400，Body 为统一 JSON。
validateSparqlInput :: API.SparqlInput -> Handler ()
validateSparqlInput (API.SparqlInput name sorts rels) = do
  let bad = throwError . (\m -> err400 { errBody = Error.encodeError 400 m })

  when (T.null $ T.strip name)  $
       bad "`snapshotName` must not be empty"

  when (null sorts) $
       bad "`sortSymbols` must contain at least one item"

  when (null rels)  $
       bad "`relations` must contain at least one element"

  -- 针对每个 RelationInput 做细项检查
  forM_ rels $ \ri -> do
    when (T.null . T.strip $ M.riRelationName ri) $
         bad "`riRelationName` cannot be empty"

    when (null $ M.riVariables ri) $
         bad "`riVariables` must list at least one variable"

    when (T.null . T.strip $ M.riQuery ri) $
         bad "`riQuery` cannot be empty"

    when (T.null . T.strip $ M.riDomainSort ri) $
         bad "`riDomainSort` cannot be empty"

    when (T.null . T.strip $ M.riRangeSort ri) $
         bad "`riRangeSort` cannot be empty"
