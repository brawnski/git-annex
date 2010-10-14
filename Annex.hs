{- git-annex monad -}

module Annex (
	new,
	run,
	gitRepo,
	gitRepoChange,
	backends,
	backendsChange,
) where

import Control.Monad.State
import qualified GitRepo as Git
import Types
import qualified BackendTypes as Backend

-- constructor
new :: Git.Repo -> AnnexState
new g = Backend.AnnexState { Backend.repo = g, Backend.backends = [] }

-- performs an action in the Annex monad
run state action = runStateT (action) state

-- Annex monad state accessors
gitRepo :: Annex Git.Repo
gitRepo = do
	state <- get
	return (Backend.repo state)
gitRepoChange :: Git.Repo -> Annex ()
gitRepoChange r = do
	state <- get
	put state { Backend.repo = r }
	return ()
backends :: Annex [Backend]
backends = do
	state <- get
	return (Backend.backends state)
backendsChange :: [Backend] -> Annex ()
backendsChange b = do
	state <- get
	put state { Backend.backends = b }
	return ()
