{- git-annex command line -}

module Commands (parseCmd) where

import System.Console.GetOpt
import Control.Monad.State (liftIO)
import System.Posix.Files
import System.Directory
import System.Path
import Data.String.Utils
import List
import IO

import qualified GitRepo as Git
import qualified Annex
import Utility
import Locations
import qualified Backend
import UUID
import LocationLog
import Types
import Core
import qualified Remotes
import qualified BackendTypes

data CmdWants = FilesInGit | FilesNotInGit | FilesInOrNotInGit |
		RepoName | SingleString
data Command = Command {
	cmdname :: String,
	cmdaction :: (String -> Annex ()),
	cmdwants :: CmdWants
}

cmds :: [Command]
cmds =  [
	  (Command "add"	addCmd		FilesNotInGit)
	, (Command "get"	getCmd		FilesInGit)
	, (Command "drop"	dropCmd		FilesInGit)
	, (Command "push"	pushCmd		RepoName)
	, (Command "pull"	pullCmd		RepoName)
	, (Command "unannex"	unannexCmd	FilesInGit)
	, (Command "describe"	describeCmd	SingleString)
	, (Command "fix"	fixCmd		FilesInOrNotInGit)
	]

options = [
	    Option ['f'] ["force"] (NoArg Force) "allow actions that may loose annexed data"
	  , Option ['N'] ["no-commit"] (NoArg NoCommit) "do not stage or commit changes"
	  ]

{- Finds the type of parameters a command wants, from among the passed
 - parameter list. -}
findWanted :: CmdWants -> [String] -> Git.Repo -> IO [String]
findWanted FilesNotInGit params repo = do
	files <- mapM (Git.notInRepo repo) params
	return $ foldl (++) [] files
findWanted FilesInGit params repo = do
	files <- mapM (Git.inRepo repo) params
	return $ foldl (++) [] files
findWanted FilesInOrNotInGit params repo = do
	a <- findWanted FilesInGit params repo
	b <- findWanted FilesNotInGit params repo
	return $ union a b
findWanted SingleString params _ = do
	return $ [unwords params]
findWanted RepoName params _ = do
	return $ params

{- Parses command line and returns a list of flags and a list of
 - actions to be run in the Annex monad. -}
parseCmd :: [String] -> AnnexState -> IO ([Flag], [Annex ()])
parseCmd argv state = do
	(flags, params) <- getopt
	case (length params) of
		0 -> error usage
		_ -> case (lookupCmd (params !! 0)) of
			[] -> error usage
			[Command _ action want] -> do
				f <- findWanted want (drop 1 params)
					(BackendTypes.repo state)
				return (flags, map action $ filter notstate f)
	where
		-- never include files from the state directory
		notstate f = stateLoc /= take (length stateLoc) f
		getopt = case getOpt Permute options argv of
			(flags, params, []) -> return (flags, params)
			(_, _, errs) -> ioError (userError (concat errs ++ usage))
		lookupCmd cmd = filter (\c -> cmd  == cmdname c) cmds
		header = "Usage: git-annex [" ++ 
			(join "|" $ map cmdname cmds) ++ "] ..."
		usage = usageInfo header options

{- Annexes a file, storing it in a backend, and then moving it into
 - the annex directory and setting up the symlink pointing to its content. -}
addCmd :: FilePath -> Annex ()
addCmd file = inBackend file err $ do
	liftIO $ checkLegal file
	g <- Annex.gitRepo
	stored <- Backend.storeFileKey file
	case (stored) of
		Nothing -> error $ "no backend could store: " ++ file
		Just (key, backend) -> do
			logStatus key ValuePresent
			setup g key
	where
		err = error $ "already annexed " ++ file
		checkLegal file = do
			s <- getSymbolicLinkStatus file
			if ((isSymbolicLink s) || (not $ isRegularFile s))
				then error $ "not a regular file: " ++ file
				else return ()
		setup g key = do
			let dest = annexLocation g key
			liftIO $ createDirectoryIfMissing True (parentDir dest)
			liftIO $ renameFile file dest
			link <- calcGitLink file key
			liftIO $ createSymbolicLink link file
			gitAdd file $ Just $ "git-annex annexed " ++ file

{- Undo addCmd. -}
unannexCmd :: FilePath -> Annex ()
unannexCmd file = notinBackend file err $ \(key, backend) -> do
	Backend.removeKey backend key
	logStatus key ValueMissing
	g <- Annex.gitRepo
	let src = annexLocation g key
	moveout g src
	where
		err = error $ "not annexed " ++ file
		moveout g src = do
			nocommit <- Annex.flagIsSet NoCommit
			liftIO $ removeFile file
			liftIO $ Git.run g ["rm", file]
			if (not nocommit)
				then liftIO $ Git.run g ["commit", "-m",
					("git-annex unannexed " ++ file), file]
				else return ()
			-- git rm deletes empty directories;
			-- put them back
			liftIO $ createDirectoryIfMissing True (parentDir file)
			liftIO $ renameFile src file
			return ()

{- Gets an annexed file from one of the backends. -}
getCmd :: FilePath -> Annex ()
getCmd file = notinBackend file err $ \(key, backend) -> do
	inannex <- inAnnex key
	if (inannex)
		then return ()
		else do
			g <- Annex.gitRepo
			let dest = annexLocation g key
			liftIO $ createDirectoryIfMissing True (parentDir dest)
			success <- Backend.retrieveKeyFile backend key dest
			if (success)
				then do
					logStatus key ValuePresent
					return ()
				else error $ "failed to get " ++ file
	where
		err = error $ "not annexed " ++ file

{- Indicates a file's content is not wanted anymore, and should be removed
 - if it's safe to do so. -}
dropCmd :: FilePath -> Annex ()
dropCmd file = notinBackend file err $ \(key, backend) -> do
	inbackend <- Backend.hasKey key
	if (not inbackend)
		then return () -- no-op
		else do
			success <- Backend.removeKey backend key
			if (success)
				then cleanup key
				else error $ "backend refused to drop " ++ file
	where
		cleanup key = do
			logStatus key ValueMissing
			inannex <- inAnnex key
			if (inannex)
				then do
					g <- Annex.gitRepo
					let loc = annexLocation g key
					liftIO $ removeFile loc
					return ()
				else return ()
		err = error $ "not annexed " ++ file

{- Fixes the symlink to an annexed file. -}
fixCmd :: String -> Annex ()
fixCmd file = notinBackend file err $ \(key, backend) -> do
	liftIO $ putStrLn $ "fix " ++ file
	link <- calcGitLink file key
	checkLegal file link
	liftIO $ createDirectoryIfMissing True (parentDir file)
	liftIO $ removeFile file
	liftIO $ createSymbolicLink link file
	gitAdd file $ Just $ "git-annex fix " ++ file
	where
		checkLegal file link = do
			l <- liftIO $ readSymbolicLink file
			if (link == l)
				then error $ "symbolic link already ok for: " ++ file
				else return ()
		err = error $ "not annexed " ++ file

{- Pushes all files to a remote repository. -}
pushCmd :: String -> Annex ()
pushCmd reponame = do error "not implemented" -- TODO

{- Pulls all files from a remote repository. -}
pullCmd :: String -> Annex ()
pullCmd reponame = do error "not implemented" -- TODO

{- Stores description for the repository. -}
describeCmd :: String -> Annex ()
describeCmd description = do
	g <- Annex.gitRepo
	u <- getUUID g
	describeUUID u description
	log <- uuidLog
	gitAdd log $ Just $ "description for UUID " ++ (show u)
	liftIO $ putStrLn "description set"

{- Updates the LocationLog when a key's presence changes. -}
logStatus :: Key -> LogStatus -> Annex ()
logStatus key status = do
	g <- Annex.gitRepo
	u <- getUUID g
	f <- liftIO $ logChange g key u status
	gitAdd f Nothing -- all logs are committed at end

inBackend file yes no = do
	r <- Backend.lookupFile file
	case (r) of
		Just v -> yes v
		Nothing -> no
notinBackend file yes no = inBackend file no yes
