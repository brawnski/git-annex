{- git-annex monad
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex (
	new,
	run,
	eval,
	gitRepo,
	gitRepoChange,
	backends,
	backendsChange,
	supportedBackends,
	flagIsSet,
	flagChange,
	flagGet,
	Flag(..),
	queue,
	queueGet,
	queueRun,
	setConfig
) where

import Control.Monad.State
import qualified Data.Map as M

import qualified GitRepo as Git
import qualified GitQueue
import Types
import qualified TypeInternals as Internals

{- Create and returns an Annex state object for the specified git repo. -}
new :: Git.Repo -> [Backend Annex] -> IO AnnexState
new gitrepo allbackends = do
	let s = Internals.AnnexState {
		Internals.repo = gitrepo,
		Internals.backends = [],
		Internals.supportedBackends = allbackends,
		Internals.flags = M.empty,
		Internals.repoqueue = GitQueue.empty,
		Internals.quiet = False
	}
	(_,s') <- Annex.run s prep
	return s'
	where
		prep = do
			-- read git config and update state
			gitrepo' <- liftIO $ Git.configRead gitrepo
			Annex.gitRepoChange gitrepo'

{- performs an action in the Annex monad -}
run :: AnnexState -> Annex a -> IO (a, AnnexState)
run state action = runStateT action state
eval :: AnnexState -> Annex a -> IO a
eval state action = evalStateT action state

{- Returns the git repository being acted on -}
gitRepo :: Annex Git.Repo
gitRepo = do
	state <- get
	return (Internals.repo state)

{- Changes the git repository being acted on. -}
gitRepoChange :: Git.Repo -> Annex ()
gitRepoChange r = do
	state <- get
	put state { Internals.repo = r }

{- Returns the backends being used. -}
backends :: Annex [Backend Annex]
backends = do
	state <- get
	return (Internals.backends state)

{- Sets the backends to use. -}
backendsChange :: [Backend Annex] -> Annex ()
backendsChange b = do
	state <- get
	put state { Internals.backends = b }

{- Returns the full list of supported backends. -}
supportedBackends :: Annex [Backend Annex]
supportedBackends = do
	state <- get
	return (Internals.supportedBackends state)

{- Return True if a Bool flag is set. -}
flagIsSet :: FlagName -> Annex Bool
flagIsSet name = do
	state <- get
	case (M.lookup name $ Internals.flags state) of
		Just (FlagBool True) -> return True
		_ -> return False 

{- Sets the value of a flag. -}
flagChange :: FlagName -> Flag -> Annex ()
flagChange name val = do
	state <- get
	put state { Internals.flags = M.insert name val $ Internals.flags state }

{- Gets the value of a String flag (or "" if there is no such String flag) -}
flagGet :: FlagName -> Annex String
flagGet name = do
	state <- get
	case (M.lookup name $ Internals.flags state) of
		Just (FlagString s) -> return s
		_ -> return ""

{- Adds a git command to the queue. -}
queue :: String -> [String] -> FilePath -> Annex ()
queue command params file = do
	state <- get
	let q = Internals.repoqueue state
	put state { Internals.repoqueue = GitQueue.add q command params file }

{- Returns the queue. -}
queueGet :: Annex GitQueue.Queue
queueGet = do
	state <- get
	return (Internals.repoqueue state)

{- Runs (and empties) the queue. -}
queueRun :: Annex ()
queueRun = do
	state <- get
	let q = Internals.repoqueue state
	g <- gitRepo
	liftIO $ GitQueue.run g q
	put state { Internals.repoqueue = GitQueue.empty }

{- Changes a git config setting in both internal state and .git/config -}
setConfig :: String -> String -> Annex ()
setConfig key value = do
	g <- Annex.gitRepo
	liftIO $ Git.run g ["config", key, value]
	-- re-read git config and update the repo's state
	g' <- liftIO $ Git.configRead g
	Annex.gitRepoChange g'
