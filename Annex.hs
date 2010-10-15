{- git-annex monad -}

module Annex (
	new,
	run,
	gitRepo,
	gitRepoChange,
	backends,
	backendsChange,
	flagIsSet,
	flagsChange,
	Flag(..)
) where

import Control.Monad.State
import qualified GitRepo as Git
import Types
import qualified BackendTypes as Backend

{- Create and returns an Annex state object for the specified git repo.
 -}
new :: Git.Repo -> IO AnnexState
new g = do
	let s = Backend.AnnexState {
		Backend.repo = g,
		Backend.backends = [],
		Backend.flags = []
	}
	(_,s') <- Annex.run s (prep g)
	return s'
	where
		prep g = do
			-- read git config and update state
			g' <- liftIO $ Git.configRead g
			Annex.gitRepoChange g'

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
flagIsSet :: Flag -> Annex Bool
flagIsSet flag = do
	state <- get
	return $ elem flag $ Backend.flags state
flagsChange :: [Flag] -> Annex ()
flagsChange b = do
	state <- get
	put state { Backend.flags = b }
	return ()
