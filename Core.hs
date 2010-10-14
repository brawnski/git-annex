{- git-annex core functions -}

module Core where

import System.IO
import System.Directory
import Control.Monad.State (liftIO)
import Control.Exception
import CmdLine
import Types
import BackendList
import Locations
import UUID
import qualified GitRepo as Git
import qualified Annex

{- Create and returns an Annex state object. 
 - Examines and prepares the git repo.
 -}
start :: IO AnnexState
start = do
	g <- Git.repoFromCwd
	let s = Annex.new g
	(_,s') <- Annex.run s (prep g)
	return s'
	where
		prep g = do
			-- setup git and read its config; update state
			g' <- liftIO $ Git.configRead g
			Annex.gitRepoChange g'
			liftIO $ gitSetup g'
			Annex.backendsChange $ parseBackendList $
				Git.configGet g' "annex.backends" ""
			prepUUID

{- Processes each param in the list by dispatching the handler function
 - for the user-selection operation mode. Catches exceptions, not stopping
 - if some error out, and propigates an overall error status at the end.
 -
 - This runs in the IO monad, not in the Annex monad. It seems that
 - exceptions can only be caught in the IO monad, not in a stacked monad;
 - or more likely I missed an easy way to do it. So, I have to laboriously
 - thread AnnexState through this function.
 -}
tryRun :: AnnexState -> Mode -> [String] -> IO ()
tryRun state mode params = tryRun' state mode 0 0 params
tryRun' state mode errnum oknum [] = do
	if (errnum > 0)
		then error $ (show errnum) ++ " failed ; " ++ show (oknum) ++ " ok"
		else return ()
tryRun' state mode errnum oknum (f:fs) = do
	result <- try
		(Annex.run state (dispatch mode f))::IO (Either SomeException ((), AnnexState))
	case (result) of
		Left err -> do
			showErr err
			tryRun' state mode (errnum + 1) oknum fs
		Right (_,state') -> tryRun' state' mode errnum (oknum + 1) fs

{- Exception pretty-printing. -}
showErr e = do
	hPutStrLn stderr $ "git-annex: " ++ (show e)
	return ()

{- Sets up a git repo for git-annex. May be called repeatedly. -}
gitSetup :: Git.Repo -> IO ()
gitSetup repo = do
	-- configure git to use union merge driver on state files
	exists <- doesFileExist attributes
	if (not exists)
		then do
			writeFile attributes $ attrLine ++ "\n"
			commit
		else do
			content <- readFile attributes
			if (all (/= attrLine) (lines content))
				then do
					appendFile attributes $ attrLine ++ "\n"
					commit
				else return ()
	where
		attrLine = stateLoc ++ "/*.log merge=union"
		attributes = Git.attributes repo
		commit = do
			Git.run repo ["add", attributes]
			Git.run repo ["commit", "-m", "git-annex setup", 
					attributes]
