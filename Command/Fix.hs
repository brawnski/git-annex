{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Fix where

import Control.Monad.State (liftIO)
import System.Posix.Files
import System.Directory

import Command
import qualified Annex
import Utility
import Core
import Messages

seek :: [SubCmdSeek]
seek = [withFilesInGit start]

{- Fixes the symlink to an annexed file. -}
start :: SubCmdStartString
start file = isAnnexed file $ \(key, _) -> do
	link <- calcGitLink file key
	l <- liftIO $ readSymbolicLink file
	if link == l
		then return Nothing
		else do
			showStart "fix" file
			return $ Just $ perform file link

perform :: FilePath -> FilePath -> SubCmdPerform
perform file link = do
	liftIO $ createDirectoryIfMissing True (parentDir file)
	liftIO $ removeFile file
	liftIO $ createSymbolicLink link file
	return $ Just $ cleanup file

cleanup :: FilePath -> SubCmdCleanup
cleanup file = do
	Annex.queue "add" ["--"] file
	return True
