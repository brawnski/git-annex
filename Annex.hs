{- git-annex monad
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex (
	Annex,
	AnnexState(..),
	getState,
	new,
	run,
	eval,
	gitRepo,
	gitRepoChange,
	backendsChange,
	FlagName,
	Flag(..),	
	flagIsSet,
	flagChange,
	flagGet,
	queue,
	queueGet,
	queueRun,
	setConfig
) where

import Control.Monad.State
import qualified Data.Map as M

import qualified GitRepo as Git
import qualified GitQueue
import qualified TypeInternals

-- git-annex's monad
type Annex = StateT AnnexState IO

-- internal state storage
data AnnexState = AnnexState {
	repo :: Git.Repo,
	backends :: [TypeInternals.Backend Annex],
	supportedBackends :: [TypeInternals.Backend Annex],
	flags :: M.Map FlagName Flag,
	repoqueue :: GitQueue.Queue,
	quiet :: Bool
} deriving (Show)

-- command-line flags
type FlagName = String
data Flag =
	FlagBool Bool |
	FlagString String
		deriving (Eq, Read, Show)

{- Create and returns an Annex state object for the specified git repo. -}
new :: Git.Repo -> [TypeInternals.Backend Annex] -> IO AnnexState
new gitrepo allbackends = do
	let s = AnnexState {
		repo = gitrepo,
		backends = [],
		supportedBackends = allbackends,
		flags = M.empty,
		repoqueue = GitQueue.empty,
		quiet = False
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

{- gets a value from the internal Annex state -}
getState :: (AnnexState -> a) -> Annex a
getState a = do
	state <- get
	return (a state)

{- Returns the git repository being acted on -}
gitRepo :: Annex Git.Repo
gitRepo = getState repo

{- Changes the git repository being acted on. -}
gitRepoChange :: Git.Repo -> Annex ()
gitRepoChange r = do
	state <- get
	put state { repo = r }

{- Sets the backends to use. -}
backendsChange :: [TypeInternals.Backend Annex] -> Annex ()
backendsChange b = do
	state <- get
	put state { backends = b }

{- Return True if a Bool flag is set. -}
flagIsSet :: FlagName -> Annex Bool
flagIsSet name = do
	state <- get
	case (M.lookup name $ flags state) of
		Just (FlagBool True) -> return True
		_ -> return False 

{- Sets the value of a flag. -}
flagChange :: FlagName -> Flag -> Annex ()
flagChange name val = do
	state <- get
	put state { flags = M.insert name val $ flags state }

{- Gets the value of a String flag (or "" if there is no such String flag) -}
flagGet :: FlagName -> Annex String
flagGet name = do
	state <- get
	case (M.lookup name $ flags state) of
		Just (FlagString s) -> return s
		_ -> return ""

{- Adds a git command to the queue. -}
queue :: String -> [String] -> FilePath -> Annex ()
queue command params file = do
	state <- get
	let q = repoqueue state
	put state { repoqueue = GitQueue.add q command params file }

{- Returns the queue. -}
queueGet :: Annex GitQueue.Queue
queueGet = do
	state <- get
	return (repoqueue state)

{- Runs (and empties) the queue. -}
queueRun :: Annex ()
queueRun = do
	state <- get
	let q = repoqueue state
	g <- gitRepo
	liftIO $ GitQueue.run g q
	put state { repoqueue = GitQueue.empty }

{- Changes a git config setting in both internal state and .git/config -}
setConfig :: String -> String -> Annex ()
setConfig key value = do
	g <- Annex.gitRepo
	liftIO $ Git.run g ["config", key, value]
	-- re-read git config and update the repo's state
	g' <- liftIO $ Git.configRead g
	Annex.gitRepoChange g'
