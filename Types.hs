{- git-annex data types
 - -}

module Types where

-- annexed filenames are mapped into keys
type Key = String

-- this structure represents a key/value backend
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

-- a git repository
data GitRepo = GitRepo {
	top :: FilePath,
	remotes :: [GitRepo]
}

-- git-annex's runtime state
data State = State {
	repo :: GitRepo,
	backends :: [Backend]
}
