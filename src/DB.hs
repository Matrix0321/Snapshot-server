{-# LANGUAGE OverloadedStrings #-}

module DB
  ( initDB
  , createTable
  , insertSnapshot
  , getAllSnapshots
  , deleteAllSnapshots
  ) where

import           Control.Monad                  (void)
import           Data.Aeson                     (encode, eitherDecode)
import qualified Data.ByteString.Lazy           as BL
import qualified Data.ByteString                as BS
import           Data.Text                      (Text)
import           Database.SQLite.Simple
  ( Connection, Query(..), Only(..), open
  , execute_, execute, query_, query, withTransaction )

import qualified Models

-- Open DB & ensure schema
initDB :: FilePath -> IO Connection
initDB fp = do
  conn <- open fp
  createTable conn
  pure conn

-- snapshotId TEXT PK, metadata JSON, relations JSON
createTable :: Connection -> IO ()
createTable conn = do
  execute_ conn $
    "CREATE TABLE IF NOT EXISTS snapshots (\
    \  snapshotId TEXT PRIMARY KEY,\
    \  metadata   BLOB NOT NULL,\
    \  relations  BLOB NOT NULL\
    \)"
  pure ()

-- upsert by PK
insertSnapshot :: Connection -> Models.Snapshot -> IO ()
insertSnapshot conn snap = withTransaction conn $ do
  let sid  = Models.unSnapshotId (Models.snapshotId snap)
      mbs  = BL.toStrict (encode (Models.metadata  snap))
      rbs  = BL.toStrict (encode (Models.relations snap))
  execute conn
    "INSERT INTO snapshots (snapshotId, metadata, relations) VALUES (?,?,?) \
    \ON CONFLICT(snapshotId) DO UPDATE SET metadata=excluded.metadata, relations=excluded.relations"
    (sid, mbs, rbs)
  pure ()

-- fetch all; skip bad rows
getAllSnapshots :: Connection -> IO [Models.Snapshot]
getAllSnapshots conn = do
  rows <- query_ conn "SELECT snapshotId, metadata, relations FROM snapshots"
            :: IO [(Text, BS.ByteString, BS.ByteString)]
  let decodeRow (sid, mb, rb) =
        case ( eitherDecode (BL.fromStrict mb)
             , eitherDecode (BL.fromStrict rb) ) of
          (Right meta, Right rels) ->
            Just (Models.Snapshot (Models.SnapshotId sid) meta rels)
          _ -> Nothing
  pure [ s | Just s <- fmap decodeRow rows ]

deleteAllSnapshots :: Connection -> IO ()
deleteAllSnapshots conn = void $ execute_ conn "DELETE FROM snapshots"
