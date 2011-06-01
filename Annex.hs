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
	gitRepo
) where

import Control.Monad.State
	(liftIO, StateT, runStateT, evalStateT, liftM, get, put)

import qualified GitRepo as Git
import qualified GitQueue
import qualified BackendClass
import qualified RemoteClass
import qualified CryptoTypes

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
	, forcebackend :: Maybe String
	, forcenumcopies :: Maybe Int
	, defaultkey :: Maybe String
	, toremote :: Maybe String
	, fromremote :: Maybe String
	, exclude :: [String]
	, cipher :: Maybe CryptoTypes.Cipher
	}

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
	, forcebackend = Nothing
	, forcenumcopies = Nothing
	, defaultkey = Nothing
	, toremote = Nothing
	, fromremote = Nothing
	, exclude = []
	, cipher = Nothing
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
changeState a = put . a =<< get

{- Returns the git repository being acted on -}
gitRepo :: Annex Git.Repo
gitRepo = getState repo
