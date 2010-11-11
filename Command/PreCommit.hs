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
import qualified Backend
import qualified GitRepo as Git
import qualified Command.Add

{- Run by git pre-commit hook; passed unlocked files that are being
 - committed. -}
start :: SubCmdStartString
start file = return $ Just $ perform file

perform :: FilePath -> SubCmdPerform
perform file = do
	pairs <- Backend.chooseBackends [file]
	ok <- doSubCmd $ Command.Add.start $ pairs !! 0
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
