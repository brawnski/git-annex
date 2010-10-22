{- git-annex internal data types
 -
 - Most things should not need this, using Types and/or Annex instead.
 -}

module TypeInternals where

import Control.Monad.State (StateT)
import Data.String.Utils
import qualified Data.Map as M

import qualified GitRepo as Git

-- command-line flags
type FlagName = String
data Flag =
	FlagBool Bool |
	FlagString String
		deriving (Eq, Read, Show)

-- git-annex's runtime state type doesn't really belong here,
-- but it uses Backend, so has to be here to avoid a depends loop.
data AnnexState = AnnexState {
	repo :: Git.Repo,
	backends :: [Backend],
	supportedBackends :: [Backend],
	flags :: M.Map FlagName Flag
} deriving (Show)

-- git-annex's monad
type Annex = StateT AnnexState IO

-- annexed filenames are mapped through a backend into keys
type KeyFrag = String
type BackendName = String
data Key = Key (BackendName, KeyFrag) deriving (Eq)

-- constructs a key in a backend
genKey :: Backend -> KeyFrag -> Key
genKey b f = Key (name b,f)

-- show a key to convert it to a string; the string includes the
-- name of the backend to avoid collisions between key strings
instance Show Key where
	show (Key (b, k)) = b ++ ":" ++ k

instance Read Key where
	readsPrec _ s = [((Key (b,k)) ,"")]
		where
			l = split ":" s
			b = l !! 0
			k = join ":" $ drop 1 l

-- pulls the backend name out
backendName :: Key -> BackendName
backendName (Key (b,k)) = b

-- this structure represents a key-value backend
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