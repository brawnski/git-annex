{- git-annex monad -}

module Annex (
	new,
	run,
	gitRepo,
	gitRepoChange,
	backends,
	backendsChange,
	supportedBackends,
	flagIsSet,
	flagChange,
	Flag(..)
) where

import Control.Monad.State

import qualified GitRepo as Git
import Types
import qualified BackendTypes as Backend

{- Create and returns an Annex state object for the specified git repo.
 -}
new :: Git.Repo -> [Backend] -> IO AnnexState
new gitrepo allbackends = do
	let s = Backend.AnnexState {
		Backend.repo = gitrepo,
		Backend.backends = [],
		Backend.supportedBackends = allbackends,
		Backend.flags = []
	}
	(_,s') <- Annex.run s (prep gitrepo)
	return s'
	where
		prep gitrepo = do
			-- read git config and update state
			gitrepo' <- liftIO $ Git.configRead gitrepo
			Annex.gitRepoChange gitrepo'

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
supportedBackends :: Annex [Backend]
supportedBackends = do
	state <- get
	return (Backend.supportedBackends state)
flagIsSet :: Flag -> Annex Bool
flagIsSet flag = do
	state <- get
	return $ elem flag $ Backend.flags state
flagChange :: Flag -> Bool -> Annex ()
flagChange flag set = do
	state <- get
	let f = filter (/= flag) $ Backend.flags state
	if (set)
		then put state { Backend.flags = (flag:f) }
		else put state { Backend.flags = f }
	return ()
