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
import qualified AnnexQueue
import qualified GitRepo as Git
import qualified Command.Add
import qualified Command.Fix
import Utility

command :: [Command]
command = [repoCommand "pre-commit" paramPath seek "run by git pre-commit hook"]

{- The pre-commit hook needs to fix symlinks to all files being committed.
 - And, it needs to inject unlocked files into the annex. -}
seek :: [CommandSeek]
seek = [withFilesToBeCommitted Command.Fix.start,
	withFilesUnlockedToBeCommitted start]

start :: CommandStartBackendFile
start pair = return $ Just $ perform pair

perform :: BackendFile -> CommandPerform
perform pair@(file, _) = do
	ok <- doCommand $ Command.Add.start pair
	if ok
		then return $ Just $ cleanup file
		else error $ "failed to add " ++ file ++ "; canceling commit"

cleanup :: FilePath -> CommandCleanup
cleanup file = do
	-- git commit will have staged the file's content;
	-- drop that and run command queued by Add.state to
	-- stage the symlink
	g <- Annex.gitRepo
	liftIO $ Git.run g "reset" [Params "-q --", File file]
	AnnexQueue.flush True
	return True
