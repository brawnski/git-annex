{- git-annex core data types -}

module Types (
	Annex,
	AnnexState,
	makeAnnexState,
	runAnnexState,
	gitAnnex,
	gitAnnexChange,
	backendsAnnex,
	backendsAnnexChange,

	Key(..),
	Backend(..)
) where

import Control.Monad.State
import Data.String.Utils
import GitRepo

-- git-annex's runtime state
data AnnexState = AnnexState {
	repo :: GitRepo,
	backends :: [Backend]
} deriving (Show)

-- git-annex's monad
type Annex = StateT AnnexState IO

-- constructor
makeAnnexState :: GitRepo -> AnnexState
makeAnnexState g = AnnexState { repo = g, backends = [] }

-- performs an action in the Annex monad
runAnnexState state action = runStateT (action) state

-- Annex monad state accessors
gitAnnex :: Annex GitRepo
gitAnnex = do
	state <- get
	return (repo state)
gitAnnexChange :: GitRepo -> Annex ()
gitAnnexChange r = do
	state <- get
	put state { repo = r }
	return ()
backendsAnnex :: Annex [Backend]
backendsAnnex = do
	state <- get
	return (backends state)
backendsAnnexChange :: [Backend] -> Annex ()
backendsAnnexChange b = do
	state <- get
	put state { backends = b }
	return ()

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
	removeKey :: Key -> Annex Bool
}

instance Show Backend where
	show backend = "Backend { name =\"" ++ (name backend) ++ "\" }"
