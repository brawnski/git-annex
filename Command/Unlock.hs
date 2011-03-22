{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Unlock where

import Control.Monad (when)
import Control.Monad.State (liftIO)
import System.Directory hiding (copyFile)

import Command
import qualified Annex
import qualified Backend
import Types
import Messages
import Locations
import Content
import CopyFile

command :: [Command]
command =
	[ repoCommand "unlock" paramPath seek "unlock files for modification"
	, repoCommand "edit" paramPath seek "same as unlock"
	]

seek :: [CommandSeek]
seek = [withFilesInGit start]

{- The unlock subcommand replaces the symlink with a copy of the file's
 - content. -}
start :: CommandStartString
start file = isAnnexed file $ \(key, _) -> do
	showStart "unlock" file
	return $ Just $ perform file key

perform :: FilePath -> Key -> CommandPerform
perform dest key = do
	inbackend <- Backend.hasKey key
	when (not inbackend) $
		error "content not present"
	
	checkDiskSpace key

	g <- Annex.gitRepo
	let src = gitAnnexLocation g key
	liftIO $ removeFile dest
	showNote "copying..."
	ok <- liftIO $ copyFile src dest
        if ok
                then do
			liftIO $ allowWrite dest
			return $ Just $ return True
                else error "copy failed!"
