{- git-annex monad
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex (
	Annex,
	AnnexState(..),
	new,
	run,
	eval,
	getState,
	changeState,
	gitRepo,
	queue,
	queueRun,
	queueRunAt,
	setConfig,
	repoConfig
) where

import Control.Monad.State
import Data.Maybe

import qualified GitRepo as Git
import qualified GitQueue
import qualified BackendClass
import qualified RemoteClass
import Utility

-- git-annex's monad
type Annex = StateT AnnexState IO

-- internal state storage
data AnnexState = AnnexState
	{ repo :: Git.Repo
	, backends :: [BackendClass.Backend Annex]
	, supportedBackends :: [BackendClass.Backend Annex]
	, remotes :: [RemoteClass.Remote Annex]
	, repoqueue :: GitQueue.Queue
	, quiet :: Bool
	, force :: Bool
	, fast :: Bool
	, defaultbackend :: Maybe String
	, defaultkey :: Maybe String
	, toremote :: Maybe String
	, fromremote :: Maybe String
	, exclude :: [String]
	} deriving (Show)

newState :: Git.Repo -> [BackendClass.Backend Annex] -> AnnexState
newState gitrepo allbackends = AnnexState
	{ repo = gitrepo
	, backends = []
	, remotes = []
	, supportedBackends = allbackends
	, repoqueue = GitQueue.empty
	, quiet = False
	, force = False
	, fast = False
	, defaultbackend = Nothing
	, defaultkey = Nothing
	, toremote = Nothing
	, fromremote = Nothing
	, exclude = []
	}

{- Create and returns an Annex state object for the specified git repo. -}
new :: Git.Repo -> [BackendClass.Backend Annex] -> IO AnnexState
new gitrepo allbackends = do
	gitrepo' <- liftIO $ Git.configRead gitrepo
	return $ newState gitrepo' allbackends

{- performs an action in the Annex monad -}
run :: AnnexState -> Annex a -> IO (a, AnnexState)
run state action = runStateT action state
eval :: AnnexState -> Annex a -> IO a
eval state action = evalStateT action state

{- Gets a value from the internal state, selected by the passed value
 - constructor. -}
getState :: (AnnexState -> a) -> Annex a
getState c = liftM c get

{- Applies a state mutation function to change the internal state. 
 -
 - Example: changeState (\s -> s { quiet = True })
 -}
changeState :: (AnnexState -> AnnexState) -> Annex ()
changeState a = do
	state <- get
	put (a state)

{- Returns the git repository being acted on -}
gitRepo :: Annex Git.Repo
gitRepo = getState repo

{- Adds a git command to the queue. -}
queue :: String -> [CommandParam] -> FilePath -> Annex ()
queue command params file = do
	state <- get
	let q = repoqueue state
	put state { repoqueue = GitQueue.add q command params file }

{- Runs (and empties) the queue. -}
queueRun :: Annex ()
queueRun = do
	state <- get
	let q = repoqueue state
	g <- gitRepo
	liftIO $ GitQueue.run g q
	put state { repoqueue = GitQueue.empty }

{- Runs the queue if the specified number of items have been queued. -}
queueRunAt :: Integer -> Annex ()
queueRunAt n = do
	state <- get
	let q = repoqueue state
	when (GitQueue.size q >= n) queueRun

{- Changes a git config setting in both internal state and .git/config -}
setConfig :: String -> String -> Annex ()
setConfig k value = do
	g <- Annex.gitRepo
	liftIO $ Git.run g "config" [Param k, Param value]
	-- re-read git config and update the repo's state
	g' <- liftIO $ Git.configRead g
	Annex.changeState $ \s -> s { Annex.repo = g' }

{- Looks up a per-remote config option in git config.
 - Failing that, tries looking for a global config option. -}
repoConfig :: Git.Repo -> String -> String -> Annex String
repoConfig r key def = do
	g <- Annex.gitRepo
	let def' = Git.configGet g global def
	return $ Git.configGet g local def'
	where
		local = "remote." ++ fromMaybe "" (Git.repoRemoteName r) ++ ".annex-" ++ key
		global = "annex." ++ key
