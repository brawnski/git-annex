{- git-annex backend data types
 - -}

module BackendType (
	-- the entire types are exported, for use in backend implementations
	Key(..),
	Backend(..)
) where

import GitRepo

-- annexed filenames are mapped into keys
type Key = FilePath

-- this structure represents a key/value backend
data Backend = Backend {
	-- name of this backend
	name :: String,
	-- converts a filename to a key
	getKey :: GitRepo -> FilePath -> IO (Maybe Key),
	-- stores a file's contents to a key
	storeFileKey :: GitRepo -> FilePath -> Key -> IO Bool,
	-- retrieves a key's contents to a file
	retrieveKeyFile :: Key -> FilePath -> IO Bool,
	-- removes a key
	removeKey :: Key -> IO Bool
}

instance Show Backend where
	show backend = "Backend { name =\"" ++ (name backend) ++ "\" }"

