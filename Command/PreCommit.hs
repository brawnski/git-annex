{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.PreCommit where

import Control.Monad.State (liftIO)

import Command
import qualified Annex
import qualified GitRepo as Git
import qualified Command.Add
import qualified Command.Fix

{- The pre-commit hook needs to fix symlinks to all files being committed.
 - And, it needs to inject unlocked files into the annex. -}
seek :: [SubCmdSeek]
seek = [withFilesToBeCommitted Command.Fix.start,
	withFilesUnlockedToBeCommitted start]

start :: SubCmdStartBackendFile
start pair = return $ Just $ perform pair

perform :: BackendFile -> SubCmdPerform
perform pair@(file, _) = do
	ok <- doSubCmd $ Command.Add.start pair
	if ok
		then return $ Just $ cleanup file
		else error $ "failed to add " ++ file ++ "; canceling commit"

cleanup :: FilePath -> SubCmdCleanup
cleanup file = do
	-- git commit will have staged the file's content;
	-- drop that and run command queued by Add.state to
	-- stage the symlink
	g <- Annex.gitRepo
	liftIO $ Git.run g ["reset", "-q", "--", file]
	Annex.queueRun
	return True
