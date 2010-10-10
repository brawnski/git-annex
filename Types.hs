{- git-annex data types
 - -}

module Types where

type Key = String

data Backend = Backend {
	-- name of this backend
	name :: String,
	-- converts a filename to a key
	getKey :: GitRepo -> FilePath -> IO (Maybe Key),
	-- stores a file's contents to a key
	storeFileKey :: GitRepo -> FilePath -> Key -> IO (Bool),
	-- retrieves a key's contents to a file
	retrieveKeyFile :: IO Key -> FilePath -> IO (Bool)
}

data GitRepo = GitRepo {
	top :: FilePath,
	remotes :: [GitRepo],
	backends :: [Backend]
}

