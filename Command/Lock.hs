{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Lock where

import Control.Monad.State (liftIO)
import System.Directory
import System.Posix.Files

import Command
import Messages
import qualified Annex
import qualified GitRepo as Git

{- Undo unlock -}
start :: SubCmdStartString
start file = do
	-- Want to avoid calling git checkout on files that are not
	-- annexed -- but without the symlink to the annex, cannot tell
	-- for sure if the file was annexed. So, check if git thinks the
	-- file's type has changed (from a symlink to a regular file).
	g <- Annex.gitRepo
	test <- liftIO $
		Git.pipeRead g ["diff", "--name-only", "--diff-filter=T", file]
	s <- liftIO $ getSymbolicLinkStatus file
	if (null test || isSymbolicLink s)
		then return Nothing
		else do
			showStart "lock" file
			return $ Just $ perform file

perform :: FilePath -> SubCmdPerform
perform file = do
	liftIO $ removeFile file
	g <- Annex.gitRepo
	liftIO $ Git.run g ["checkout", file]
	return $ Just $ return True -- no cleanup needed
