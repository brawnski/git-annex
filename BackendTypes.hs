{- git-annex backend data types
 -
 - Mostly only backend implementations should need to import this.
 -}

module BackendTypes where

import Control.Monad.State
import Data.String.Utils
import qualified GitRepo as Git

-- git-annex's runtime state type doesn't really belong here,
-- but it uses Backend, so has to be here to avoid a depends loop.
data AnnexState = AnnexState {
	repo :: Git.Repo,
	backends :: [Backend]
} deriving (Show)

-- git-annex's monad
type Annex = StateT AnnexState IO

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
	getKey :: FilePath -> Annex (Maybe Key),
	-- stores a file's contents to a key
	storeFileKey :: FilePath -> Key -> Annex Bool,
	-- retrieves a key's contents to a file
	retrieveKeyFile :: Key -> FilePath -> Annex Bool,
	-- removes a key
	removeKey :: Key -> Annex Bool,
	-- checks if a backend is storing the content of a key
	hasKey :: Key -> Annex Bool
}

instance Show Backend where
	show backend = "Backend { name =\"" ++ (name backend) ++ "\" }"
