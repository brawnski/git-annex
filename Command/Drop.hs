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
import Content
import Messages
import Utility

command :: [Command]
command = [repoCommand "drop" paramPath seek
	"indicate content of files not currently wanted"]

seek :: [CommandSeek]
seek = [withAttrFilesInGit "annex.numcopies" start]

{- Indicates a file's content is not wanted anymore, and should be removed
 - if it's safe to do so. -}
start :: CommandStartAttrFile
start (file, attr) = isAnnexed file $ \(key, backend) -> do
	inbackend <- Backend.hasKey key
	if inbackend
		then do
			showStart "drop" file
			next $ perform key backend numcopies
		else stop
	where
		numcopies = readMaybe attr :: Maybe Int

perform :: Key -> Backend Annex -> Maybe Int -> CommandPerform
perform key backend numcopies = do
	success <- Backend.removeKey backend key numcopies
	if success
		then next $ cleanup key
		else stop

cleanup :: Key -> CommandCleanup
cleanup key = do
	inannex <- inAnnex key
	when inannex $ removeAnnex key
	logStatus key ValueMissing
	return True
