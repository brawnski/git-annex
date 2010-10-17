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

data CmdWants = FilesInGit | FilesNotInGit | RepoName | SingleString
data Command = Command {
	cmdname :: String,
	cmdaction :: (String -> Annex ()),
	cmdwants :: CmdWants,
	cmddesc :: String
}

cmds :: [Command]
cmds =  [
	  (Command "add"	addCmd		FilesNotInGit
		"add files to annex")
	, (Command "get"	getCmd		FilesInGit
		"make content of annexed files available")
	, (Command "drop"	dropCmd		FilesInGit
		"indicate content of files not currently wanted")
	, (Command "unannex"	unannexCmd	FilesInGit
		"undo accidential add command")
	, (Command "init"	initCmd		SingleString
		"initialize git-annex with repository description")
	, (Command "fix"	fixCmd		FilesInGit
		"fix up files' symlinks to point to annexed content")
	]

options = [
	    Option ['f'] ["force"] (NoArg Force) "allow actions that may loose annexed data"
	  , Option ['N'] ["no-commit"] (NoArg NoCommit) "do not stage or commit changes"
	  ]

header = "Usage: git-annex [" ++ (join "|" $ map cmdname cmds) ++ "] ..."

usage = usageInfo header options ++ "\nSubcommands:\n" ++ cmddescs
	where
		cmddescs = unlines $ map (\c -> indent $ showcmd c) cmds
		showcmd c =
			(cmdname c) ++
			(take (10 - (length (cmdname c))) $ repeat ' ') ++ 
			(cmddesc c)
		indent l = "  " ++ l

{- Finds the type of parameters a command wants, from among the passed
 - parameter list. -}
findWanted :: CmdWants -> [String] -> Git.Repo -> IO [String]
findWanted FilesNotInGit params repo = do
	files <- mapM (Git.notInRepo repo) params
	return $ foldl (++) [] files
findWanted FilesInGit params repo = do
	files <- mapM (Git.inRepo repo) params
	return $ foldl (++) [] files
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
			[Command _ action want _] -> do
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

{- Annexes a file, storing it in a backend, and then moving it into
 - the annex directory and setting up the symlink pointing to its content. -}
addCmd :: FilePath -> Annex ()
addCmd file = inBackend file $ do
	liftIO $ checkLegal file
	showStart "add" file
	g <- Annex.gitRepo
	stored <- Backend.storeFileKey file
	case (stored) of
		Nothing -> showEndFail "no backend could store" file
		Just (key, backend) -> do
			logStatus key ValuePresent
			setup g key
	where
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
			showEndOk

{- Undo addCmd. -}
unannexCmd :: FilePath -> Annex ()
unannexCmd file = notinBackend file $ \(key, backend) -> do
	showStart "unannex" file
	Annex.flagChange Force True -- force backend to always remove
	Backend.removeKey backend key
	logStatus key ValueMissing
	g <- Annex.gitRepo
	let src = annexLocation g key
	moveout g src
	where
		moveout g src = do
			nocommit <- Annex.flagIsSet NoCommit
			liftIO $ removeFile file
			liftIO $ Git.run g ["rm", "--quiet", file]
			if (not nocommit)
				then liftIO $ Git.run g ["commit", "--quiet",
					"-m", ("git-annex unannexed " ++ file),
					file]
				else return ()
			-- git rm deletes empty directories;
			-- put them back
			liftIO $ createDirectoryIfMissing True (parentDir file)
			liftIO $ renameFile src file
			showEndOk

{- Gets an annexed file from one of the backends. -}
getCmd :: FilePath -> Annex ()
getCmd file = notinBackend file $ \(key, backend) -> do
	inannex <- inAnnex key
	if (inannex)
		then return ()
		else do
			showStart "get" file
			g <- Annex.gitRepo
			let dest = annexLocation g key
			let tmp = (annexTmpLocation g) ++ (keyFile key)
			liftIO $ createDirectoryIfMissing True (parentDir tmp)
			success <- Backend.retrieveKeyFile backend key tmp
			if (success)
				then do
					liftIO $ renameFile tmp dest	
					logStatus key ValuePresent
					showEndOk
				else do
					showEndFail "get" file

{- Indicates a file's content is not wanted anymore, and should be removed
 - if it's safe to do so. -}
dropCmd :: FilePath -> Annex ()
dropCmd file = notinBackend file $ \(key, backend) -> do
	inbackend <- Backend.hasKey key
	if (not inbackend)
		then return () -- no-op
		else do
			showStart "drop" file
			success <- Backend.removeKey backend key
			if (success)
				then do
					cleanup key
					showEndOk
				else showEndFail "backend refused to drop" file
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

{- Fixes the symlink to an annexed file. -}
fixCmd :: String -> Annex ()
fixCmd file = notinBackend file $ \(key, backend) -> do
	link <- calcGitLink file key
	l <- liftIO $ readSymbolicLink file
	if (link == l)
		then return ()
		else do
			showStart "fix" file
			liftIO $ createDirectoryIfMissing True (parentDir file)
			liftIO $ removeFile file
			liftIO $ createSymbolicLink link file
			gitAdd file $ Just $ "git-annex fix " ++ file
			showEndOk

{- Stores description for the repository. -}
initCmd :: String -> Annex ()
initCmd description = do
	if (0 == length description)
		then error $ 
			"please specify a description of this repository\n" ++
			usage
		else do
			g <- Annex.gitRepo
			u <- getUUID g
			describeUUID u description
			log <- uuidLog
			gitAdd log $ Just $ "description for UUID " ++ (show u)
			liftIO $ putStrLn "description set"

-- helpers
inBackend file a = do
	r <- Backend.lookupFile file
	case (r) of
		Just v -> return ()
		Nothing -> a
notinBackend file a = do
	r <- Backend.lookupFile file
	case (r) of
		Just v -> a v
		Nothing -> return ()
