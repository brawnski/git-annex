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
	flagGet,
	Flag(..)
) where

import Control.Monad.State
import qualified Data.Map as M

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
		Internals.flags = M.empty
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
flagIsSet :: FlagName -> Annex Bool
flagIsSet name = do
	state <- get
	case (M.lookup name $ Internals.flags state) of
		Just (FlagBool True) -> return True
		_ -> return False 
flagChange :: FlagName -> Flag -> Annex ()
flagChange name val = do
	state <- get
	put state { Internals.flags = M.insert name val $ Internals.flags state }
	return ()
flagGet :: FlagName -> Annex String
flagGet name = do
	state <- get
	case (M.lookup name $ Internals.flags state) of
		Just (FlagString s) -> return s
		_ -> return ""
