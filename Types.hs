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
	storeFileKey :: GitRepo -> FilePath -> Key -> IO Bool,
	-- retrieves a key's contents to a file
	retrieveKeyFile :: Key -> FilePath -> IO Bool,
	-- removes a key
	removeKey :: Key -> IO Bool
}

-- a git repository
data GitRepo = GitRepo {
	top :: FilePath,
	bare :: Bool
}

-- git-annex's runtime state
data State = State {
	repo :: GitRepo,
	gitconfig :: GitConfig
}

data GitConfig = GitConfig {
	annex_name :: String,
	annex_numcopies :: Int,
	annex_backends :: [Backend]
}
