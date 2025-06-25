{-# LANGUAGE OverloadedStrings #-}

module DB
  ( initDB
  , insertSnapshot
  , getAllSnapshots
  , deleteAllSnapshots
  ) where

import Database.SQLite.Simple
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString      as BS
import Data.Aeson   (encode, decode)
import qualified Models as M
import qualified Data.Map.Strict ()
import Control.Monad (forM)
import Data.Maybe    (fromMaybe)

initDB :: IO Connection
initDB = do
  conn <- open "snapshots.db"
  execute_ conn
    "CREATE TABLE IF NOT EXISTS snapshots (\
    \ snapshotId TEXT PRIMARY KEY,\
    \ metadata   BLOB,\
    \ relations  BLOB)"
  pure conn

insertSnapshot :: Connection -> M.Snapshot -> IO ()
insertSnapshot conn (M.Snapshot sid meta rels) = do
  let metaBs = BL.toStrict (encode meta)
      relsBs = BL.toStrict (encode rels)
  execute conn
    "INSERT OR REPLACE INTO snapshots (snapshotId, metadata, relations) VALUES (?, ?, ?)"
    (sid, metaBs, relsBs)

getAllSnapshots :: Connection -> IO [M.Snapshot]
getAllSnapshots conn = do
  rows <- query_ conn
            "SELECT snapshotId, metadata, relations FROM snapshots"
            :: IO [(Text, BS.ByteString, BS.ByteString)]
  forM rows $ \(sid, mb, rb) -> do
    let meta = fromMaybe (error "bad meta") (decode $ BL.fromStrict mb)
        rels = fromMaybe (error "bad rels") (decode $ BL.fromStrict rb)
    pure (M.Snapshot sid meta rels)

deleteAllSnapshots :: Connection -> IO ()
deleteAllSnapshots conn = execute_ conn "DELETE FROM snapshots"
