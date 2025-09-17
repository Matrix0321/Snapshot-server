{-# LANGUAGE OverloadedStrings #-}

module Validation
  ( validateSparqlInput
  ) where

import           Control.Monad              (when, forM_)
import           Control.Monad.Except       (throwError)
import qualified Data.Text                  as T
import           Servant.Server             (Handler, err400, errBody)

import qualified API
import           Models                     (RelationInput(..))
import           Error                      (encodeError)

validateSparqlInput :: API.SparqlInput -> Handler ()
validateSparqlInput (API.SparqlInput name sorts rels) = do
  let bad :: String -> Handler a
      bad msg = throwError $ err400 { errBody = encodeError 400 msg }

  when (T.null $ T.strip name) $
    bad "`snapshotName` must not be empty"

  when (null sorts) $
    bad "`sortSymbols` must contain at least one item"

  when (null rels) $
    bad "`relations` must contain at least one element"

  forM_ rels $ \ri -> do
    when (T.null . T.strip $ riRelationName ri) $
      bad "`riRelationName` cannot be empty"
    when (null $ riVariables ri) $
      bad "`riVariables` must list at least one variable"
    when (T.null . T.strip $ riQuery ri) $
      bad "`riQuery` cannot be empty"
    when (T.null . T.strip $ riDomainSort ri) $
      bad "`riDomainSort` cannot be empty"
    when (T.null . T.strip $ riRangeSort ri) $
      bad "`riRangeSort` cannot be empty"
