{-# LANGUAGE OverloadedStrings #-}

module SnapshotGenerator
  ( generateSnapshot
  ) where

import           Control.Exception            (try, SomeException)
import           Control.Monad                (forM)
import           Data.UUID.V4                 (nextRandom)
import           Data.UUID                    (toText)
import           Data.Time                    (getCurrentTime, formatTime, defaultTimeLocale)
import qualified Data.Text                    as T
import qualified Data.Map.Strict              as Map
import qualified Data.ByteString.Lazy.Char8  as BL8

import           Network.HTTP.Client          (Response, responseBody)

import           Database.SPARQL.Protocol.Client
  ( select
  , SelectResult(..)
  , RDFTerm(..)
  )
import           Database.SQLite.Simple       (Connection)

import qualified DB
import qualified Models
import qualified API

-- Extract text from RDFTerm (best-effort)
extractText :: Maybe RDFTerm -> T.Text
extractText (Just (LiteralLang t _)) = t
extractText (Just (Literal t))       = t
extractText (Just (LiteralType t _)) = t
extractText (Just (IRI uri))         = uri
extractText (Just (Blank b))         = b
extractText _                        = T.empty

-- Execute SPARQL, build Snapshot, and insert into DB.
generateSnapshot
  :: Connection
  -> API.SparqlInput
  -> IO (Either T.Text (Models.Snapshot, API.SnapshotCreated))
generateSnapshot conn (API.SparqlInput name sortSyms relInputs) = do
  newId  <- toText <$> nextRandom
  nowUtc <- getCurrentTime
  let createdAt = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" nowUtc
      endpoint  = "https://query.wikidata.org/sparql"

  relResults <- forM relInputs $ \ri -> do
    let q       = Models.riQuery ri
        relName = Models.riRelationName ri
        dom     = Models.riDomainSort ri
        rng     = Models.riRangeSort ri
        vars    = Models.riVariables ri

    eres <- try (select endpoint (BL8.pack $ T.unpack q))
             :: IO (Either SomeException (Response SelectResult))

    case eres of
      Left ex ->
        pure . Left $ "SPARQL query failed (" <> relName <> "): " <> T.pack (show ex)
      Right resp ->
        let SelectResult rows = responseBody resp
            facts = [ Map.fromList [ (v, extractText (Map.lookup v r)) | v <- vars ]
                    | r <- rows
                    ]
        in pure $ Right (relName, facts, dom, rng, vars)

  case sequence relResults of
    Left errMsg -> pure $ Left errMsg
    Right triples -> do
      let relationsMap = Map.fromList [ (n, fs) | (n, fs, _, _, _) <- triples ]
          relSigs      = [ Models.RelationSignature n vs d r | (n, _, d, r, vs) <- triples ]
          meta = Models.SnapshotMetadata
            { Models.snapshotName       = name
            , Models.author             = "System"
            , Models.createdAt          = createdAt
            , Models.description        = T.intercalate "; " [ n | (n, _, _, _, _) <- triples ]
            , Models.signatureId        = "wikidata-auto"
            , Models.sortSymbols        = sortSyms
            , Models.relationSignatures = relSigs
            }
          snap = Models.Snapshot (Models.SnapshotId newId) meta relationsMap

      eresIns <- try (DB.insertSnapshot conn snap) :: IO (Either SomeException ())
      case eresIns of
        Left ex  -> pure . Left $ "Database insert failed: " <> T.pack (show ex)
        Right () -> pure $ Right (snap, API.SnapshotCreated newId)
