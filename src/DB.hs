{-# LANGUAGE OverloadedStrings #-}

module DB
  ( initDB
  , insertSnapshot
  , getAllSnapshots
  , deleteAllSnapshots
  ) where

import           Database.SQLite.Simple
import           Data.Text                      (Text)
import qualified Data.ByteString.Lazy           as BL
import qualified Data.ByteString                as BS
import           Data.Aeson                     (encode, eitherDecode)
import qualified Data.Map.Strict                as Map
import           Control.Monad                  (forM)
import           System.IO                      (hPutStrLn, stderr)

import qualified Models                         as Models

initDB :: IO Connection
initDB = do
  conn <- open "snapshots.db"
  execute_ conn
    "CREATE TABLE IF NOT EXISTS snapshots (\
    \ snapshotId TEXT PRIMARY KEY,\
    \ metadata   BLOB,\
    \ relations  BLOB)"
  pure conn

insertSnapshot :: Connection -> Models.Snapshot -> IO ()
insertSnapshot conn (Models.Snapshot sid meta rels) = do
  let metaBs = BL.toStrict (encode meta)
      relsBs = BL.toStrict (encode rels)
      sidTxt = case sid of Models.SnapshotId t -> t
  execute conn
    "INSERT OR REPLACE INTO snapshots (snapshotId, metadata, relations) VALUES (?, ?, ?)"
    (sidTxt, metaBs, relsBs)

-- We return IO [Snapshot]. On decode failure of a row, we log and skip.
getAllSnapshots :: Connection -> IO [Models.Snapshot]
getAllSnapshots conn = do
  rows <- query_ conn
            "SELECT snapshotId, metadata, relations FROM snapshots"
            :: IO [(Text, BS.ByteString, BS.ByteString)]
  fmap concat $ forM rows $ \(sidTxt, mb, rb) -> do
    let metaE = eitherDecode (BL.fromStrict mb) :: Either String Models.SnapshotMetadata
        relsE = eitherDecode (BL.fromStrict rb) :: Either String (Map.Map Models.RelationName [Map.Map Models.Variable Text])
    case (metaE, relsE) of
      (Right meta, Right rels) ->
        pure [Models.Snapshot (Models.SnapshotId sidTxt) meta rels]
      (Left em, _) -> hPutStrLn stderr ("[DB] Failed to decode metadata for " ++ show sidTxt ++ ": " ++ em) >> pure []
      (_, Left er) -> hPutStrLn stderr ("[DB] Failed to decode relations for " ++ show sidTxt ++ ": " ++ er) >> pure []

deleteAllSnapshots :: Connection -> IO ()
deleteAllSnapshots conn = execute_ conn "DELETE FROM snapshots"
