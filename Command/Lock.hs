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

import Types
import Command
import Messages
import qualified Annex
import qualified GitRepo as Git

{- Undo unlock -}
start :: SubCmdStartString
start file = do
	locked <- isLocked file
	if locked
		then return Nothing
		else do
			showStart "lock" file
			return $ Just $ perform file

perform :: FilePath -> SubCmdPerform
perform file = do
	liftIO $ removeFile file
	g <- Annex.gitRepo
	-- first reset the file to drop any changes checked into the index
	liftIO $ Git.run g ["reset", "-q", "--", file]
	-- checkout the symlink
	liftIO $ Git.run g ["checkout", "--", file]
	return $ Just $ return True -- no cleanup needed

{- Checks if a file is unlocked for edit.
 -
 - But, without the symlink to the annex, cannot tell for sure if the
 - file was annexed before. So, check if git thinks the file's type has
 - changed (from a symlink to a regular file). -}
isLocked :: FilePath -> Annex Bool
isLocked file = do
	g <- Annex.gitRepo
	changed <- typechanged g Nothing
	changedCached <- typechanged g $ Just "--cached"
	s <- liftIO $ getSymbolicLinkStatus file
	return $ null (changed++changedCached) || isSymbolicLink s
	where
		typechanged g Nothing = typechanged' g params
		typechanged g (Just param) = typechanged' g $ params++[param]
		typechanged' g p = liftIO $ Git.pipeRead g $ p++[file]
		params = ["diff", "--name-only", "--diff-filter=T"]
