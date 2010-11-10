{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.PreCommit where

import Control.Monad.State (liftIO)
import Control.Monad (when, unless)

import Command
import qualified Annex
import qualified Backend
import qualified GitRepo as Git
import qualified Command.Fix
import qualified Command.Lock
import qualified Command.Add

{- Run by git pre-commit hook. -}
start :: SubCmdStartString
start file = do
	-- If a file is unlocked for edit, add its new content to the
	-- annex, -}
	locked <- Command.Lock.isLocked file
	when (not locked) $ do
		pairs <- Backend.chooseBackends [file]
		ok <- doSubCmd $ Command.Add.start $ pairs !! 0
		unless (ok) $ do
			error $ "failed to add " ++ file ++ "; canceling commit"
		-- git commit will have staged the file's content;
		-- drop that and stage the symlink
		g <- Annex.gitRepo
		liftIO $ Git.run g ["reset", "-q", "--", file]
		liftIO $ Git.run g ["add", "--", file]

	-- Fix symlinks as they are committed, this ensures the
	-- relative links are not broken when moved around.
	Command.Fix.start file
