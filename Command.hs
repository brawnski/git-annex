{- git-annex commands
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command where

import Control.Monad.State (liftIO)
import System.Directory
import System.Posix.Files
import Control.Monad (filterM)
import System.Path.WildMatch
import Text.Regex.PCRE.Light.Char8
import Data.List

import Types
import qualified Backend
import Messages
import qualified Annex
import qualified GitRepo as Git
import Locations

{- A command runs in four stages.
 -
 - 0. The seek stage takes the parameters passed to the command,
 -    looks through the repo to find the ones that are relevant
 -    to that command (ie, new files to add), and generates
 -    a list of start stage actions. -}
type CommandSeek = [String] -> Annex [CommandStart]
{- 1. The start stage is run before anything is printed about the
  -   command, is passed some input, and can early abort it
  -   if the input does not make sense. It should run quickly and
  -   should not modify Annex state. -}
type CommandStart = Annex (Maybe CommandPerform)
{- 2. The perform stage is run after a message is printed about the command
 -    being run, and it should be where the bulk of the work happens. -}
type CommandPerform = Annex (Maybe CommandCleanup)
{- 3. The cleanup stage is run only if the perform stage succeeds, and it
 -    returns the overall success/fail of the command. -}
type CommandCleanup = Annex Bool
{- Some helper functions are used to build up CommandSeek and CommandStart
 - functions. -}
type CommandSeekStrings = CommandStartString -> CommandSeek
type CommandStartString = String -> CommandStart
type BackendFile = (FilePath, Maybe (Backend Annex))
type CommandSeekBackendFiles = CommandStartBackendFile -> CommandSeek
type CommandStartBackendFile = BackendFile -> CommandStart
type AttrFile = (FilePath, String)
type CommandSeekAttrFiles = CommandStartAttrFile -> CommandSeek
type CommandStartAttrFile = AttrFile -> CommandStart
type CommandSeekNothing = CommandStart -> CommandSeek
type CommandStartNothing = CommandStart

data Command = Command {
	cmdname :: String,
	cmdparams :: String,
	cmdseek :: [CommandSeek],
	cmddesc :: String
}

{- Prepares a list of actions to run to perform a command, based on
 - the parameters passed to it. -}
prepCmd :: Command -> [String] -> Annex [Annex Bool]
prepCmd Command { cmdseek = seek } params = do
	lists <- mapM (\s -> s params) seek
	return $ map doCommand $ concat lists

{- Runs a command through the start, perform and cleanup stages -}
doCommand :: CommandStart -> CommandCleanup
doCommand start = do
	s <- start
	case s of
		Nothing -> return True
		Just perform -> do
			p <- perform
			case p of
				Nothing -> do
					showEndFail
					return False
				Just cleanup -> do
					c <- cleanup
					if c
						then do
							showEndOk
							return True
						else do
							showEndFail
							return False

notAnnexed :: FilePath -> Annex (Maybe a) -> Annex (Maybe a)
notAnnexed file a = do
	r <- Backend.lookupFile file
	case r of
		Just _ -> return Nothing
		Nothing -> a

isAnnexed :: FilePath -> ((Key, Backend Annex) -> Annex (Maybe a)) -> Annex (Maybe a)
isAnnexed file a = do
	r <- Backend.lookupFile file
	case r of
		Just v -> a v
		Nothing -> return Nothing

{- These functions find appropriate files or other things based on a
   user's parameters, and run a specified action on them. -}
withFilesInGit :: CommandSeekStrings
withFilesInGit a params = do
	repo <- Annex.gitRepo
	files <- liftIO $ Git.inRepo repo params
	files' <- filterFiles files
	return $ map a files'
withAttrFilesInGit :: String -> CommandSeekAttrFiles
withAttrFilesInGit attr a params = do
	repo <- Annex.gitRepo
	files <- liftIO $ Git.inRepo repo params
	files' <- filterFiles files
	pairs <- liftIO $ Git.checkAttr repo attr files'
	return $ map a pairs
withBackendFilesInGit :: CommandSeekBackendFiles
withBackendFilesInGit a params = do
	repo <- Annex.gitRepo
	files <- liftIO $ Git.inRepo repo params
	files' <- filterFiles files
	backendPairs a files'
withFilesMissing :: CommandSeekStrings
withFilesMissing a params = do
	files <- liftIO $ filterM missing params
	files' <- filterFiles files
	return $ map a files'
	where
		missing f = do
			e <- doesFileExist f
			return $ not e
withFilesNotInGit :: CommandSeekBackendFiles
withFilesNotInGit a params = do
	repo <- Annex.gitRepo
	newfiles <- liftIO $ Git.notInRepo repo params
	newfiles' <- filterFiles newfiles
	backendPairs a newfiles'
withString :: CommandSeekStrings
withString a params = return [a $ unwords params]
withStrings :: CommandSeekStrings
withStrings a params = return $ map a params
withFilesToBeCommitted :: CommandSeekStrings
withFilesToBeCommitted a params = do
	repo <- Annex.gitRepo
	tocommit <- liftIO $ Git.stagedFiles repo params
	tocommit' <- filterFiles tocommit
	return $ map a tocommit'
withFilesUnlocked :: CommandSeekBackendFiles
withFilesUnlocked = withFilesUnlocked' Git.typeChangedFiles
withFilesUnlockedToBeCommitted :: CommandSeekBackendFiles
withFilesUnlockedToBeCommitted = withFilesUnlocked' Git.typeChangedStagedFiles
withFilesUnlocked' :: (Git.Repo -> [FilePath] -> IO [FilePath]) -> CommandSeekBackendFiles
withFilesUnlocked' typechanged a params = do
	-- unlocked files have changed type from a symlink to a regular file
	repo <- Annex.gitRepo
	typechangedfiles <- liftIO $ typechanged repo params
	unlockedfiles <- liftIO $ filterM notSymlink $
		map (\f -> Git.workTree repo ++ "/" ++ f) typechangedfiles
	unlockedfiles' <- filterFiles unlockedfiles
	backendPairs a unlockedfiles'
withKeys :: CommandSeekStrings
withKeys a params = return $ map a params
withTempFile :: CommandSeekStrings
withTempFile a params = return $ map a params
withNothing :: CommandSeekNothing
withNothing a [] = return [a]
withNothing _ _ = return []

backendPairs :: CommandSeekBackendFiles
backendPairs a files = do
	pairs <- Backend.chooseBackends files
	return $ map a pairs

{- Filter out files from the state directory, and those matching the
 - exclude glob pattern, if it was specified. -}
filterFiles :: [FilePath] -> Annex [FilePath]
filterFiles l = do
	let l' = filter notState l
	exclude <- Annex.getState Annex.exclude
	if null exclude
		then return l'
		else do
			let regexp = compile (toregex exclude) []
			return $ filter (notExcluded regexp) l'
	where
		notState f = not $ stateDir `isPrefixOf` f
		notExcluded r f = case match r f [] of
			Nothing -> True
			Just _ -> False
		toregex exclude = "^(" ++ toregex' exclude "" ++ ")"
		toregex' [] c = c
		toregex' (w:ws) "" = toregex' ws (wildToRegex w)
		toregex' (w:ws) c = toregex' ws (c ++ "|" ++ wildToRegex w)

{- filter out symlinks -}	
notSymlink :: FilePath -> IO Bool
notSymlink f = do
	s <- liftIO $ getSymbolicLinkStatus f
	return $ not $ isSymbolicLink s

{- Descriptions of params used in usage messages. -}
paramRepeating :: String -> String
paramRepeating s = s ++ " ..."
paramOptional :: String -> String
paramOptional s = "[" ++ s ++ "]"
paramPath :: String
paramPath = "PATH"
paramKey :: String
paramKey = "KEY"
paramDesc :: String
paramDesc = "DESCRIPTION"
paramNumber :: String
paramNumber = "NUMBER"
paramRemote :: String
paramRemote = "REMOTE"
paramGlob :: String
paramGlob = "GLOB"
paramName :: String
paramName = "NAME"
paramNothing :: String
paramNothing = ""

{- The Key specified by the --key and --backend parameters. -}
cmdlineKey :: Annex Key
cmdlineKey  = do
	k <- Annex.getState Annex.defaultkey
	backends <- Backend.list
	return $ genKey (head backends) (keyname' k)
	where
		keyname' Nothing = badkey
		keyname' (Just "") = badkey
		keyname' (Just n) = n
		badkey = error "please specify the key with --key"

