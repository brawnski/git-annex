{- git-annex command line -}

module Commands (parseCmd) where

import System.Console.GetOpt
import Control.Monad.State (liftIO)
import System.Posix.Files
import System.Directory
import System.Path
import Data.String.Utils
import Control.Monad (filterM)
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
import qualified TypeInternals

data CmdWants = FilesInGit | FilesNotInGit | FilesMissing | Description
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
	, (Command "move"	moveCmd		FilesInGit
		"transfer content of files to/from another repository")
	, (Command "init"	initCmd		Description
		"initialize git-annex with repository description")
	, (Command "unannex"	unannexCmd	FilesInGit
		"undo accidential add command")
	, (Command "fix"	fixCmd		FilesInGit
		"fix up files' symlinks to point to annexed content")
	, (Command "fromkey"	fromKeyCmd	FilesMissing
		"adds a file using a specific key")
	]

-- Each dashed command-line option results in generation of an action
-- in the Annex monad that performs the necessary setting.
options :: [OptDescr (Annex ())]
options = [
	    Option ['f'] ["force"] (NoArg (storebool "force" True))
		"allow actions that may lose annexed data"
	  , Option ['b'] ["backend"] (ReqArg (storestring "backend") "NAME")
		"specify default key-value backend to use"
	  , Option ['k'] ["key"] (ReqArg (storestring "key") "KEY")
		"specify a key to use"
	  , Option ['t'] ["to"] (ReqArg (storestring "torepository") "REPOSITORY")
		"specify to where to transfer content"
	  , Option ['f'] ["from"] (ReqArg (storestring "fromrepository") "REPOSITORY")
		"specify from where to transfer content"
	  ]
	where
		storebool n b = Annex.flagChange n $ FlagBool b
		storestring n s = Annex.flagChange n $ FlagString s

header = "Usage: git-annex " ++ (join "|" $ map cmdname cmds)

usage :: String
usage = usageInfo header options ++ "\nSubcommands:\n" ++ cmddescs
	where
		cmddescs = unlines $ map (\c -> indent $ showcmd c) cmds
		showcmd c =
			(cmdname c) ++
			(pad 10 (cmdname c)) ++
			(descWanted (cmdwants c)) ++
			(pad 13 (descWanted (cmdwants c))) ++
			(cmddesc c)
		indent l = "  " ++ l
		pad n s = take (n - (length s)) $ repeat ' '

{- Generate descrioptions of wanted parameters for subcommands. -}
descWanted :: CmdWants -> String
descWanted Description = "DESCRIPTION"
descWanted _ = "PATH ..."

{- Finds the type of parameters a command wants, from among the passed
 - parameter list. -}
findWanted :: CmdWants -> [String] -> Git.Repo -> IO [String]
findWanted FilesNotInGit params repo = do
	files <- mapM (Git.notInRepo repo) params
	return $ foldl (++) [] files
findWanted FilesInGit params repo = do
	files <- mapM (Git.inRepo repo) params
	return $ foldl (++) [] files
findWanted FilesMissing params repo = do
	files <- liftIO $ filterM missing params
	return $ files
	where
		missing f = do
			e <- doesFileExist f
			if (e) then return False else return True
findWanted Description params _ = do
	return $ [unwords params]

{- Parses command line and returns two lists of actions to be 
 - run in the Annex monad. The first actions configure it
 - according to command line options, while the second actions
 - handle subcommands. -}
parseCmd :: [String] -> AnnexState -> IO ([Annex ()], [Annex ()])
parseCmd argv state = do
	(flags, params) <- getopt
	if (null params)
		then error usage
		else case (lookupCmd (params !! 0)) of
			[] -> error usage
			[Command _ action want _] -> do
				f <- findWanted want (drop 1 params)
					(TypeInternals.repo state)
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
addCmd file = notAnnexed file $ do
	s <- liftIO $ getSymbolicLinkStatus file
	if ((isSymbolicLink s) || (not $ isRegularFile s))
		then return ()
		else do
			showStart "add" file
			g <- Annex.gitRepo
			stored <- Backend.storeFileKey file
			case (stored) of
				Nothing -> showEndFail
				Just (key, backend) -> do
					logStatus key ValuePresent
					setup g key
	where
		setup g key = do
			let dest = annexLocation g key
			liftIO $ createDirectoryIfMissing True (parentDir dest)
			liftIO $ renameFile file dest
			link <- calcGitLink file key
			liftIO $ createSymbolicLink link file
			liftIO $ Git.run g ["add", file]
			showEndOk

{- Undo addCmd. -}
unannexCmd :: FilePath -> Annex ()
unannexCmd file = isAnnexed file $ \(key, backend) -> do
	showStart "unannex" file
	Annex.flagChange "force" $ FlagBool True -- force backend to always remove
	Backend.removeKey backend key
	logStatus key ValueMissing
	g <- Annex.gitRepo
	let src = annexLocation g key
	moveout g src
	where
		moveout g src = do
			liftIO $ removeFile file
			liftIO $ Git.run g ["rm", "--quiet", file]
			-- git rm deletes empty directories;
			-- put them back
			liftIO $ createDirectoryIfMissing True (parentDir file)
			liftIO $ renameFile src file
			showEndOk

{- Gets an annexed file from one of the backends. -}
getCmd :: FilePath -> Annex ()
getCmd file = isAnnexed file $ \(key, backend) -> do
	inannex <- inAnnex key
	if (inannex)
		then return ()
		else do
			showStart "get" file
			getViaTmp key (Backend.retrieveKeyFile backend key)

{- Indicates a file's content is not wanted anymore, and should be removed
 - if it's safe to do so. -}
dropCmd :: FilePath -> Annex ()
dropCmd file = isAnnexed file $ \(key, backend) -> do
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
				else showEndFail
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
fixCmd :: FilePath -> Annex ()
fixCmd file = isAnnexed file $ \(key, backend) -> do
	link <- calcGitLink file key
	l <- liftIO $ readSymbolicLink file
	if (link == l)
		then return ()
		else do
			showStart "fix" file
			liftIO $ createDirectoryIfMissing True (parentDir file)
			liftIO $ removeFile file
			liftIO $ createSymbolicLink link file
			g <- Annex.gitRepo
			liftIO $ Git.run g ["add", file]
			showEndOk

{- Stores description for the repository. -}
initCmd :: String -> Annex ()
initCmd description = do
	if (null description)
		then error $ 
			"please specify a description of this repository\n" ++
			usage
		else do
			g <- Annex.gitRepo
			u <- getUUID g
			describeUUID u description
			log <- uuidLog
			liftIO $ Git.run g ["add", log]
			liftIO $ Git.run g ["commit", "-m", "git annex init", log]
			liftIO $ putStrLn "description set"

{- Adds a file pointing at a manually-specified key -}
fromKeyCmd :: FilePath -> Annex ()
fromKeyCmd file = do
	keyname <- Annex.flagGet "key"
	if (null keyname)
		then error "please specify the key with --key"
		else return ()
	backends <- Backend.list
	let key = genKey (backends !! 0) keyname

	inbackend <- Backend.hasKey key
	if (not inbackend)
		then error $ "key ("++keyname++") is not present in backend"
		else return ()

	link <- calcGitLink file key
	showStart "fromkey" file
	liftIO $ createDirectoryIfMissing True (parentDir file)
	liftIO $ createSymbolicLink link file
	g <- Annex.gitRepo
	liftIO $ Git.run g ["add", file]
	showEndOk

{- Move a file either --to or --from a repository.
 -
 - This only operates on the cached file content; it does not involve
 - moving data in the key-value backend.
 -
 - Note that unlike drop, this does not honor annex.numcopies.
 - A file's content can be moved even if there are insufficient copies to
 - allow it to be dropped.
 -}
moveCmd :: FilePath -> Annex ()
moveCmd file = do
	fromName <- Annex.flagGet "fromrepository"
	toName <- Annex.flagGet "torepository"
	case (fromName, toName) of
		("", "") -> error "specify either --from or --to"
		("", to) -> moveTo file
		(from, "") -> moveFrom file
		(_, _) -> error "only one of --from or --to can be specified"

{- Moves the content of an annexed file to another repository,
 - removing it from the current repository, and updates locationlog
 - information on both.
 -
 - If the destination already has the content, it is still removed
 - from the current repository.
 -}
moveTo :: FilePath -> Annex ()
moveTo file = isAnnexed file $ \(key, backend) -> do
	ishere <- inAnnex key
	if (not ishere)
		then return () -- not here, so nothing to do
		else do
			showStart "move" file
			remote <- Remotes.commandLineRemote
			isthere <- Remotes.inAnnex remote key
			case isthere of
				Left err -> error (show err)
				Right True -> removeit
				Right False -> moveit
	where
		moveit = do
			error $ "TODO move" ++ file
		removeit = do
			error $ "TODO remove" ++ file

{- Moves the content of an annexed file from another repository to the current
 - repository and updates locationlog information on both.
 -
 - If the current repository already has the content, it is still removed
 - from the other repository.
 -}
moveFrom :: FilePath -> Annex ()
moveFrom file = isAnnexed file $ \(key, backend) -> do
	showStart "move" file -- have to show this before checking remote
	ishere <- inAnnex key
	remote <- Remotes.commandLineRemote
	isthere <- Remotes.inAnnex remote key
	case (ishere, isthere) of
		(_, Left err) -> error (show err)
		(_, Right False) -> showEndFail
		(False, Right True) -> moveit remote key
		(True, Right True) -> removeit remote key
	where
		moveit remote key = do
			getViaTmp key (Remotes.copyFromRemote remote key)
			removeit remote key
		removeit remote key = do
			error $ "TODO remove" ++ file
			showEndOk

-- helpers
notAnnexed file a = do
	r <- Backend.lookupFile file
	case (r) of
		Just v -> return ()
		Nothing -> a
isAnnexed file a = do
	r <- Backend.lookupFile file
	case (r) of
		Just v -> a v
		Nothing -> return ()
