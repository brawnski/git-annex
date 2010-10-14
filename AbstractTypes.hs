{- git-annex data types, abstract only -}

module AbstractTypes (
	Annex,
	AnnexState,
	makeAnnexState,
	runAnnexState,
	gitAnnex,
	gitAnnexChange,
	backendsAnnex,
	backendsAnnexChange,

	Key,
	Backend
) where

import Control.Monad.State
import qualified GitRepo as Git
import BackendTypes

-- constructor
makeAnnexState :: Git.Repo -> AnnexState
makeAnnexState g = AnnexState { repo = g, backends = [] }

-- performs an action in the Annex monad
runAnnexState state action = runStateT (action) state

-- Annex monad state accessors
gitAnnex :: Annex Git.Repo
gitAnnex = do
	state <- get
	return (repo state)
gitAnnexChange :: Git.Repo -> Annex ()
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

