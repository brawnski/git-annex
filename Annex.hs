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
	setConfig
) where

import Control.Monad.State

import qualified GitRepo as Git
import qualified GitQueue
import qualified TypeInternals

-- git-annex's monad
type Annex = StateT AnnexState IO

-- internal state storage
data AnnexState = AnnexState
	{ repo :: Git.Repo
	, backends :: [TypeInternals.Backend Annex]
	, supportedBackends :: [TypeInternals.Backend Annex]
	, repoqueue :: GitQueue.Queue
	, quiet :: Bool
	, force :: Bool
	, defaultbackend :: Maybe String
	, defaultkey :: Maybe String
	, toremote :: Maybe String
	, fromremote :: Maybe String
	, exclude :: [String]
	, remotesread :: Bool
	} deriving (Show)

newState :: Git.Repo -> [TypeInternals.Backend Annex] -> AnnexState
newState gitrepo allbackends = AnnexState
	{ repo = gitrepo
	, backends = []
	, supportedBackends = allbackends
	, repoqueue = GitQueue.empty
	, quiet = False
	, force = False
	, defaultbackend = Nothing
	, defaultkey = Nothing
	, toremote = Nothing
	, fromremote = Nothing
	, exclude = []
	, remotesread = False
	}

{- Create and returns an Annex state object for the specified git repo. -}
new :: Git.Repo -> [TypeInternals.Backend Annex] -> IO AnnexState
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
getState c = do
	state <- get
	return (c state)

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
queue :: String -> [String] -> FilePath -> Annex ()
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

{- Changes a git config setting in both internal state and .git/config -}
setConfig :: String -> String -> Annex ()
setConfig k value = do
	g <- Annex.gitRepo
	liftIO $ Git.run g ["config", k, value]
	-- re-read git config and update the repo's state
	g' <- liftIO $ Git.configRead g
	Annex.changeState $ \s -> s { Annex.repo = g' }
