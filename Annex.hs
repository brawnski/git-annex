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
import qualified TypeInternals as Internals

{- Create and returns an Annex state object for the specified git repo.
 -}
new :: Git.Repo -> [Backend] -> IO AnnexState
new gitrepo allbackends = do
	let s = Internals.AnnexState {
		Internals.repo = gitrepo,
		Internals.backends = [],
		Internals.supportedBackends = allbackends,
		Internals.flags = []
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
	return (Internals.repo state)
gitRepoChange :: Git.Repo -> Annex ()
gitRepoChange r = do
	state <- get
	put state { Internals.repo = r }
	return ()
backends :: Annex [Backend]
backends = do
	state <- get
	return (Internals.backends state)
backendsChange :: [Backend] -> Annex ()
backendsChange b = do
	state <- get
	put state { Internals.backends = b }
	return ()
supportedBackends :: Annex [Backend]
supportedBackends = do
	state <- get
	return (Internals.supportedBackends state)
flagIsSet :: Flag -> Annex Bool
flagIsSet flag = do
	state <- get
	return $ elem flag $ Internals.flags state
flagChange :: Flag -> Bool -> Annex ()
flagChange flag set = do
	state <- get
	let f = filter (/= flag) $ Internals.flags state
	if (set)
		then put state { Internals.flags = (flag:f) }
		else put state { Internals.flags = f }
	return ()
