{- git-annex core functions
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Core where

import IO (try)
import System.IO
import System.Directory
import Control.Monad.State (liftIO)
import System.Path
import Data.String.Utils
import Control.Monad (when, unless)

import Types
import Locations
import LocationLog
import UUID
import qualified GitRepo as Git
import qualified GitQueue
import qualified Annex
import Utility

{- Runs a list of Annex actions. Catches IO errors and continues
 - (but explicitly thrown errors terminate the whole command).
 - Propigates an overall error status at the end.
 -}
tryRun :: AnnexState -> [Annex Bool] -> IO ()
tryRun state actions = tryRun' state 0 actions
tryRun' :: AnnexState -> Integer -> [Annex Bool] -> IO ()
tryRun' state errnum (a:as) = do
	result <- try $ Annex.run state a
	case (result) of
		Left err -> do
			Annex.eval state $ showErr err
			tryRun' state (errnum + 1) as
		Right (True,state') -> tryRun' state' errnum as
		Right (False,state') -> tryRun' state' (errnum + 1) as
tryRun' _ errnum [] =
	when (errnum > 0) $ error $ show errnum ++ " failed"
			
{- Sets up a git repo for git-annex. -}
startup :: Annex Bool
startup = do
	prepUUID
	return True

{- When git-annex is done, it runs this. -}
shutdown :: Annex Bool
shutdown = do
	g <- Annex.gitRepo

	-- Runs all queued git commands.
	q <- Annex.queueGet
	unless (q == GitQueue.empty) $ do
		verbose $ liftIO $ putStrLn "Recording state in git..."
		liftIO $ GitQueue.run g q

	-- clean up any files left in the temp directory, but leave
	-- the tmp directory itself
	let tmp = annexTmpLocation g
	exists <- liftIO $ doesDirectoryExist tmp
	when (exists) $ liftIO $ removeDirectoryRecursive tmp
	liftIO $ createDirectoryIfMissing True tmp

	return True

{- configure git to use union merge driver on state files, if it is not
 - already -}
gitAttributes :: Git.Repo -> IO ()
gitAttributes repo = do
	exists <- doesFileExist attributes
	if (not exists)
		then do
			writeFile attributes $ attrLine ++ "\n"
			commit
		else do
			content <- readFile attributes
			when (all (/= attrLine) (lines content)) $ do
				appendFile attributes $ attrLine ++ "\n"
				commit
	where
		attrLine = stateLoc ++ "*.log merge=union"
		attributes = Git.attributes repo
		commit = do
			Git.run repo ["add", attributes]
			Git.run repo ["commit", "-m", "git-annex setup", 
					attributes]

{- set up a git pre-commit hook, if one is not already present -}
gitPreCommitHook :: Git.Repo -> IO ()
gitPreCommitHook repo = do
	let hook = Git.workTree repo ++ "/" ++ Git.gitDir repo ++
		"/hooks/pre-commit"
	exists <- doesFileExist hook
	if (exists)
		then putStrLn $ "pre-commit hook (" ++ hook ++ ") already exists, not configuring"
		else do
			writeFile hook $ "#!/bin/sh\n" ++
				"# automatically configured by git-annex\n" ++ 
				"git annex pre-commit .\n"
			p <- getPermissions hook
			setPermissions hook $ p {executable = True}

{- Checks if a given key is currently present in the annexLocation. -}
inAnnex :: Key -> Annex Bool
inAnnex key = do
	g <- Annex.gitRepo
	when (Git.repoIsUrl g) $ error "inAnnex cannot check remote repo"
	liftIO $ doesFileExist $ annexLocation g key

{- Calculates the relative path to use to link a file to a key. -}
calcGitLink :: FilePath -> Key -> Annex FilePath
calcGitLink file key = do
	g <- Annex.gitRepo
	cwd <- liftIO $ getCurrentDirectory
	let absfile = case (absNormPath cwd file) of
		Just f -> f
		Nothing -> error $ "unable to normalize " ++ file
	return $ relPathDirToDir (parentDir absfile) (Git.workTree g) ++
		annexLocationRelative key

{- Updates the LocationLog when a key's presence changes. -}
logStatus :: Key -> LogStatus -> Annex ()
logStatus key status = do
	g <- Annex.gitRepo
	u <- getUUID g
	logfile <- liftIO $ logChange g key u status
	Annex.queue "add" [] logfile

{- Runs an action, passing it a temporary filename to download,
 - and if the action succeeds, moves the temp file into 
 - the annex as a key's content. -}
getViaTmp :: Key -> (FilePath -> Annex Bool) -> Annex Bool
getViaTmp key action = do
	g <- Annex.gitRepo
	let dest = annexLocation g key
	let tmp = annexTmpLocation g ++ keyFile key
	liftIO $ createDirectoryIfMissing True (parentDir tmp)
	success <- action tmp
	if (success)
		then do
			liftIO $ renameFile tmp dest
			logStatus key ValuePresent
			return True
		else do
			-- the tmp file is left behind, in case caller wants
			-- to resume its transfer
			return False

{- Output logging -}
verbose :: Annex () -> Annex ()
verbose a = do
	q <- Annex.flagIsSet "quiet"
	unless q a
showStart :: String -> String -> Annex ()
showStart command file = verbose $ do
	liftIO $ putStr $ command ++ " " ++ file ++ " "
	liftIO $ hFlush stdout
showNote :: String -> Annex ()
showNote s = verbose $ do
	liftIO $ putStr $ "(" ++ s ++ ") "
	liftIO $ hFlush stdout
showProgress :: Annex ()
showProgress = verbose $ liftIO $ putStr "\n"
showLongNote :: String -> Annex ()
showLongNote s = verbose $ do
	liftIO $ putStr $ "\n" ++ indented
	where
		indented = join "\n" $ map (\l -> "  " ++ l) $ lines s 
showEndOk :: Annex ()
showEndOk = verbose $ do
	liftIO $ putStrLn "ok"
showEndFail :: Annex ()
showEndFail = verbose $ do
	liftIO $ putStrLn "\nfailed"

{- Exception pretty-printing. -}
showErr :: (Show a) => a -> Annex ()
showErr e = warning $ show e

warning :: String -> Annex ()
warning s = liftIO $ hPutStrLn stderr $ "git-annex: " ++ s
