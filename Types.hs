{- git-annex core data types -}

module Types (
	State(..),
	Key(..),
	Backend(..)
) where

import Data.String.Utils
import GitRepo

-- git-annex's runtime state
data State = State {
	repo :: GitRepo,
	backends :: [Backend]
} deriving (Show)

-- annexed filenames are mapped into keys
data Key = Key String deriving (Eq)

-- show a key to convert it to a string
instance Show Key where
	show (Key v) = v

-- this structure represents a key/value backend
data Backend = Backend {
	-- name of this backend
	name :: String,
	-- converts a filename to a key
	getKey :: State -> FilePath -> IO (Maybe Key),
	-- stores a file's contents to a key
	storeFileKey :: State -> FilePath -> Key -> IO Bool,
	-- retrieves a key's contents to a file
	retrieveKeyFile :: State -> Key -> FilePath -> IO Bool,
	-- removes a key
	removeKey :: State -> Key -> IO Bool
}

instance Show Backend where
	show backend = "Backend { name =\"" ++ (name backend) ++ "\" }"
