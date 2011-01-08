{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Add where

import Control.Monad.State (liftIO)
import System.Posix.Files

import Command
import qualified Annex
import qualified Backend
import LocationLog
import Types
import Core
import Messages

command :: [Command]
command = [Command "add" paramPath seek "add files to annex"]

{- Add acts on both files not checked into git yet, and unlocked files. -}
seek :: [CommandSeek]
seek = [withFilesNotInGit start, withFilesUnlocked start]

{- The add subcommand annexes a file, storing it in a backend, and then
 - moving it into the annex directory and setting up the symlink pointing
 - to its content. -}
start :: CommandStartBackendFile
start pair@(file, _) = notAnnexed file $ do
	s <- liftIO $ getSymbolicLinkStatus file
	if (isSymbolicLink s) || (not $ isRegularFile s)
		then return Nothing
		else do
			showStart "add" file
			return $ Just $ perform pair

perform :: BackendFile -> CommandPerform
perform (file, backend) = do
	stored <- Backend.storeFileKey file backend
	case stored of
		Nothing -> return Nothing
		Just (key, _) -> do
			moveAnnex key file
			return $ Just $ cleanup file key

cleanup :: FilePath -> Key -> CommandCleanup
cleanup file key = do
	logStatus key ValuePresent

	link <- calcGitLink file key
	liftIO $ createSymbolicLink link file
	Annex.queue "add" ["--"] file
	return True
