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

{- A subcommand runs in three stages. Each stage can return the next stage
 - to run.
 -
 - 1. The start stage is run before anything is printed about the
 -   subcommand, and can early abort it if the input does not make sense.
 -   It should run quickly and should not modify Annex state.
 -
 - 2. The perform stage is run after a message is printed about the subcommand
 -    being run, and it should be where the bulk of the work happens.
 -
 - 3. The cleanup stage is run only if the perform stage succeeds, and it
 -    returns the overall success/fail of the subcommand.
 -}
type SubCmdStart = String -> Annex (Maybe SubCmdPerform)
type SubCmdPerform = Annex (Maybe SubCmdCleanup)
type SubCmdCleanup = Annex Bool

{- Runs a subcommand through its three stages. -}
doSubCmd :: String -> SubCmdStart -> String -> Annex ()
doSubCmd cmdname start param = do
	res <- start param :: Annex (Maybe SubCmdPerform)
	case (res) of
		Nothing -> return ()
		Just perform -> do
			showStart cmdname param
			res <- perform :: Annex (Maybe SubCmdCleanup)
			case (res) of
				Nothing -> showEndFail
				Just cleanup -> do
					res <- cleanup
					if (res)
						then showEndOk
						else showEndFail


{- A subcommand can broadly want one of several kinds of input parameters.
 - This allows a first stage of filtering before starting a subcommand. -}
data SubCmdWants = FilesInGit | FilesNotInGit | FilesMissing
	| Description | Keys

data SubCommand = Command {
	subcmdname :: String,
	subcmdaction :: SubCmdStart,
	subcmdwants :: SubCmdWants,
	subcmddesc :: String
}
subCmds :: [SubCommand]
subCmds =  [
	  (Command "add"	addStart	FilesNotInGit
		"add files to annex")
	, (Command "get"	getStart	FilesInGit
		"make content of annexed files available")
	, (Command "drop"	dropStart	FilesInGit
		"indicate content of files not currently wanted")
	, (Command "move"	moveStart	FilesInGit
		"transfer content of files to/from another repository")
	, (Command "init"	initStart	Description
		"initialize git-annex with repository description")
	, (Command "unannex"	unannexStart	FilesInGit
		"undo accidential add command")
	, (Command "fix"	fixStart	FilesInGit
		"fix up files' symlinks to point to annexed content")
	, (Command "fromkey"	fromKeyStart	FilesMissing
		"adds a file using a specific key")
	, (Command "dropkey"	fromKeyStart	Keys
		"drops cached content for specified keys")
	]

-- Each dashed command-line option results in generation of an action
-- in the Annex monad that performs the necessary setting.
options :: [OptDescr (Annex ())]
options = [
	    Option ['f'] ["force"] (NoArg (storebool "force" True))
		"allow actions that may lose annexed data"
	  , Option ['q'] ["quiet"] (NoArg (storebool "quiet" True))
		"avoid verbose output"
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

header = "Usage: git-annex " ++ (join "|" $ map subcmdname subCmds)

{- Usage message with lists of options and subcommands. -}
usage :: String
usage = usageInfo header options ++ "\nSubcommands:\n" ++ cmddescs
	where
		cmddescs = unlines $ map (\c -> indent $ showcmd c) subCmds
		showcmd c =
			(subcmdname c) ++
			(pad 10 (subcmdname c)) ++
			(descWanted (subcmdwants c)) ++
			(pad 13 (descWanted (subcmdwants c))) ++
			(subcmddesc c)
		indent l = "  " ++ l
		pad n s = take (n - (length s)) $ repeat ' '

{- Generate descriptions of wanted parameters for subcommands. -}
descWanted :: SubCmdWants -> String
descWanted Description = "DESCRIPTION"
descWanted Keys = "KEY ..."
descWanted _ = "PATH ..."

{- Finds the type of parameters a subcommand wants, from among the passed
 - parameter list. -}
findWanted :: SubCmdWants -> [String] -> Git.Repo -> IO [String]
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
findWanted Keys params _ = return params

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
			[Command name action want _] -> do
				f <- findWanted want (drop 1 params)
					(TypeInternals.repo state)
				return (flags, map (doSubCmd name action) $
					filter notstate f)
	where
		-- never include files from the state directory
		notstate f = stateLoc /= take (length stateLoc) f
		getopt = case getOpt Permute options argv of
			(flags, params, []) -> return (flags, params)
			(_, _, errs) -> ioError (userError (concat errs ++ usage))
		lookupCmd cmd = filter (\c -> cmd  == subcmdname c) subCmds

{- The add subcommand annexes a file, storing it in a backend, and then
 - moving it into the annex directory and setting up the symlink pointing
 - to its content. -}
addStart :: FilePath -> Annex (Maybe SubCmdPerform)
addStart file = notAnnexed file $ do
	s <- liftIO $ getSymbolicLinkStatus file
	if ((isSymbolicLink s) || (not $ isRegularFile s))
		then return Nothing
		else return $ Just $ addPerform file
addPerform :: FilePath -> Annex (Maybe SubCmdCleanup)
addPerform file = do
	g <- Annex.gitRepo
	stored <- Backend.storeFileKey file
	case (stored) of
		Nothing -> return Nothing
		Just (key, backend) -> return $ Just $ addCleanup file key
addCleanup :: FilePath -> Key -> Annex Bool
addCleanup file key = do
	logStatus key ValuePresent
	g <- Annex.gitRepo
	let dest = annexLocation g key
	liftIO $ createDirectoryIfMissing True (parentDir dest)
	liftIO $ renameFile file dest
	link <- calcGitLink file key
	liftIO $ createSymbolicLink link file
	liftIO $ Git.run g ["add", file]
	return True

{- The unannex subcommand undoes an add. -}
unannexStart :: FilePath -> Annex (Maybe SubCmdPerform)
unannexStart file = isAnnexed file $ \(key, backend) -> do
	return $ Just $ unannexPerform file key backend
unannexPerform :: FilePath -> Key -> Backend -> Annex (Maybe SubCmdCleanup)
unannexPerform file key backend = do
	-- force backend to always remove
	Annex.flagChange "force" $ FlagBool True
	Backend.removeKey backend key
	return $ Just $ unannexCleanup file key
unannexCleanup :: FilePath -> Key -> Annex Bool
unannexCleanup file key = do
	logStatus key ValueMissing
	g <- Annex.gitRepo
	let src = annexLocation g key
	liftIO $ removeFile file
	liftIO $ Git.run g ["rm", "--quiet", file]
	-- git rm deletes empty directories; put them back
	liftIO $ createDirectoryIfMissing True (parentDir file)
	liftIO $ renameFile src file
	return True

{- Gets an annexed file from one of the backends. -}
getStart :: FilePath -> Annex (Maybe SubCmdPerform)
getStart file = isAnnexed file $ \(key, backend) -> do
	inannex <- inAnnex key
	if (inannex)
		then return Nothing
		else return $ Just $ getPerform file key backend
getPerform :: FilePath -> Key -> Backend -> Annex (Maybe SubCmdCleanup)
getPerform file key backend = do
	ok <- getViaTmp key (Backend.retrieveKeyFile backend key)
	if (ok)
		then return $ Just $ return True
		else return Nothing

{- Indicates a file's content is not wanted anymore, and should be removed
 - if it's safe to do so. -}
dropStart :: FilePath -> Annex (Maybe SubCmdPerform)
dropStart file = isAnnexed file $ \(key, backend) -> do
	inbackend <- Backend.hasKey key
	if (not inbackend)
		then return Nothing
		else return $ Just $ dropPerform key backend
dropPerform :: Key -> Backend -> Annex (Maybe SubCmdCleanup)
dropPerform key backend = do
	success <- Backend.removeKey backend key
	if (success)
		then return $ Just $ dropCleanup key
		else return Nothing
dropCleanup :: Key -> Annex Bool
dropCleanup key = do
	logStatus key ValueMissing
	inannex <- inAnnex key
	if (inannex)
		then do
			g <- Annex.gitRepo
			let loc = annexLocation g key
			liftIO $ removeFile loc
			return True
		else return True

{- Drops cached content for a key. -}
dropKeyStart :: String -> Annex (Maybe SubCmdPerform)
dropKeyStart keyname = do
	backends <- Backend.list
	let key = genKey (backends !! 0) keyname
	present <- inAnnex key
	force <- Annex.flagIsSet "force"
	if (not present)
		then return Nothing
		else if (not force)
			then error "dropkey is can cause data loss; use --force if you're sure you want to do this"
			else return $ Just $ dropKeyPerform key
dropKeyPerform :: Key -> Annex (Maybe SubCmdCleanup)
dropKeyPerform key = do
	g <- Annex.gitRepo
	let loc = annexLocation g key
	liftIO $ removeFile loc
	return $ Just $ dropKeyCleanup key
dropKeyCleanup :: Key -> Annex Bool
dropKeyCleanup key = do
	logStatus key ValueMissing
	return True

{- Fixes the symlink to an annexed file. -}
fixStart :: FilePath -> Annex (Maybe SubCmdPerform)
fixStart file = isAnnexed file $ \(key, backend) -> do
	link <- calcGitLink file key
	l <- liftIO $ readSymbolicLink file
	if (link == l)
		then return Nothing
		else return $ Just $ fixPerform file link
fixPerform :: FilePath -> FilePath -> Annex (Maybe SubCmdCleanup)
fixPerform file link = do
	liftIO $ createDirectoryIfMissing True (parentDir file)
	liftIO $ removeFile file
	liftIO $ createSymbolicLink link file
	g <- Annex.gitRepo
	liftIO $ Git.run g ["add", file]
	return $ Just $ fixCleanup
fixCleanup :: Annex Bool
fixCleanup = do
	return True

{- Stores description for the repository. -}
initStart :: String -> Annex (Maybe SubCmdPerform)
initStart description = do
	if (null description)
		then error $ 
			"please specify a description of this repository\n" ++
			usage
		else return $ Just $ initPerform description
initPerform :: String -> Annex (Maybe SubCmdCleanup)
initPerform description = do
	g <- Annex.gitRepo
	u <- getUUID g
	describeUUID u description
	return $ Just $ initCleanup
initCleanup :: Annex Bool
initCleanup = do
	g <- Annex.gitRepo
	log <- uuidLog
	liftIO $ Git.run g ["add", log]
	liftIO $ Git.run g ["commit", "-m", "git annex init", log]
	return True

{- Adds a file pointing at a manually-specified key -}
fromKeyStart :: FilePath -> Annex (Maybe SubCmdPerform)
fromKeyStart file = do
	keyname <- Annex.flagGet "key"
	if (null keyname)
		then error "please specify the key with --key"
		else return ()
	backends <- Backend.list
	let key = genKey (backends !! 0) keyname

	inbackend <- Backend.hasKey key
	if (not inbackend)
		then error $ "key ("++keyname++") is not present in backend"
		else return $ Just $ fromKeyPerform file key
fromKeyPerform :: FilePath -> Key -> Annex (Maybe SubCmdCleanup)
fromKeyPerform file key = do
	link <- calcGitLink file key
	liftIO $ createDirectoryIfMissing True (parentDir file)
	liftIO $ createSymbolicLink link file
	return $ Just $ fromKeyCleanup file
fromKeyCleanup :: FilePath -> Annex Bool
fromKeyCleanup file = do
	g <- Annex.gitRepo
	liftIO $ Git.run g ["add", file]
	return True

{- Move a file either --to or --from a repository.
 -
 - This only operates on the cached file content; it does not involve
 - moving data in the key-value backend. -}
moveStart :: FilePath -> Annex (Maybe SubCmdPerform)
moveStart file = do
	fromName <- Annex.flagGet "fromrepository"
	toName <- Annex.flagGet "torepository"
	case (fromName, toName) of
		("", "") -> error "specify either --from or --to"
		("", to) -> moveToStart file
		(from, "") -> moveFromStart file
		(_, _) -> error "only one of --from or --to can be specified"

{- Moves the content of an annexed file to another repository,
 - removing it from the current repository, and updates locationlog
 - information on both.
 -
 - If the destination already has the content, it is still removed
 - from the current repository.
 -
 - Note that unlike drop, this does not honor annex.numcopies.
 - A file's content can be moved even if there are insufficient copies to
 - allow it to be dropped.
 -}
moveToStart :: FilePath -> Annex (Maybe SubCmdPerform)
moveToStart file = isAnnexed file $ \(key, backend) -> do
	ishere <- inAnnex key
	if (not ishere)
		then return Nothing -- not here, so nothing to do
		else return $ Just $ moveToPerform file key
moveToPerform :: FilePath -> Key -> Annex (Maybe SubCmdCleanup)
moveToPerform file key = do
	-- checking the remote is expensive, so not done in the start step
	remote <- Remotes.commandLineRemote
	isthere <- Remotes.inAnnex remote key
	case isthere of
		Left err -> do
			showNote $ show err
			return Nothing
		Right False -> do
			ok <- Remotes.copyToRemote remote key
			if (ok)
				then return $ Just $ moveToCleanup remote key
				else return Nothing -- failed
		Right True -> return $ Just $ moveToCleanup remote key
moveToCleanup :: Git.Repo -> Key -> Annex Bool
moveToCleanup remote key = do
	-- cleanup on the local side is the same as done for the drop subcommand
	ok <- dropCleanup key
	if (not ok)
		then return False
		else do
			-- Record that the key is present on the remote.
			u <- getUUID remote
			liftIO $ logChange remote key u ValuePresent
			-- Propigate location log to remote.
			error "TODO: update remote locationlog"
			return True

{- Moves the content of an annexed file from another repository to the current
 - repository and updates locationlog information on both.
 -
 - If the current repository already has the content, it is still removed
 - from the other repository.
 -}
moveFromStart :: FilePath -> Annex (Maybe SubCmdPerform)
moveFromStart file = isAnnexed file $ \(key, backend) -> do
	return $ Just $ moveFromPerform file key
moveFromPerform :: FilePath -> Key -> Annex (Maybe SubCmdCleanup)
moveFromPerform file key = do
	-- checking the remote is expensive, so not done in the start step
	remote <- Remotes.commandLineRemote
	isthere <- Remotes.inAnnex remote key
	ishere <- inAnnex key
	case (ishere, isthere) of
		(_, Left err) -> do
			showNote $ show err
			return Nothing
		(_, Right False) -> do
			showNote $ "not present in " ++ (Git.repoDescribe remote)
			return Nothing
		(False, Right True) -> do
			-- copy content from remote
			ok <- getViaTmp key (Remotes.copyFromRemote remote key)
			if (ok)
				then return $ Just $ moveFromCleanup remote key
				else return Nothing -- fail
		(True, Right True) -> do
			-- the content is already here, just remove from remote
			return $ Just $ moveFromCleanup remote key
moveFromCleanup :: Git.Repo -> Key -> Annex Bool
moveFromCleanup remote key = do
	-- Force drop content from the remote.
	Remotes.runCmd remote "git-annex" ["dropkey", "--quiet", "--force",
		"--backend=" ++ (backendName key),
		keyName key]
	-- Record locally that the key is not on the remote.
	remoteuuid <- getUUID remote
	g <- Annex.gitRepo
	liftIO $ logChange g key remoteuuid ValueMissing
	return True

-- helpers
notAnnexed file a = do
	r <- Backend.lookupFile file
	case (r) of
		Just v -> return Nothing
		Nothing -> a
isAnnexed file a = do
	r <- Backend.lookupFile file
	case (r) of
		Just v -> a v
		Nothing -> return Nothing
