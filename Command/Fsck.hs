{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Fsck where

import Command
import qualified Backend
import Types
import Messages

seek :: [SubCmdSeek]
seek = [withAll withFilesInGit start]

{- Checks a file's backend data for problems. -}
start :: SubCmdStartString
start file = isAnnexed file $ \(key, backend) -> do
	showStart "fsck" file
	return $ Just $ perform key backend

perform :: Key -> Backend -> SubCmdPerform
perform key backend = do
	success <- Backend.fsckKey backend key
	if (success)
		then return $ Just $ return True
		else return Nothing
