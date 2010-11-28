{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Unlock where

import Control.Monad.State (liftIO)
import System.Directory hiding (copyFile)

import Command
import qualified Annex
import Types
import Messages
import Locations
import Core
import CopyFile

seek :: [SubCmdSeek]
seek = [withFilesInGit start]

{- The unlock subcommand replaces the symlink with a copy of the file's
 - content. -}
start :: SubCmdStartString
start file = isAnnexed file $ \(key, _) -> do
	showStart "unlock" file
	return $ Just $ perform file key

perform :: FilePath -> Key -> SubCmdPerform
perform dest key = do
	g <- Annex.gitRepo
	let src = annexLocation g key
	liftIO $ removeFile dest
	showNote "copying..."
	ok <- liftIO $ copyFile src dest
        if ok
                then do
			liftIO $ allowWrite dest
			return $ Just $ return True
                else error "copy failed!"
