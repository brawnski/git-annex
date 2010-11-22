{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Drop where

import Control.Monad (when)

import Command
import qualified Backend
import LocationLog
import Types
import Core
import Messages

seek :: [SubCmdSeek]
seek = [withFilesInGit start]

{- Indicates a file's content is not wanted anymore, and should be removed
 - if it's safe to do so. -}
start :: SubCmdStartString
start file = isAnnexed file $ \(key, backend) -> do
	inbackend <- Backend.hasKey key
	if not inbackend
		then return Nothing
		else do
			showStart "drop" file
			return $ Just $ perform key backend

perform :: Key -> Backend -> SubCmdPerform
perform key backend = do
	success <- Backend.removeKey backend key
	if success
		then return $ Just $ cleanup key
		else return Nothing

cleanup :: Key -> SubCmdCleanup
cleanup key = do
	inannex <- inAnnex key
	when inannex $ removeAnnex key
	logStatus key ValueMissing
	return True
