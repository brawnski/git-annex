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
import Utility

command :: [Command]
command = [Command "fsck" (paramOptional $ paramRepeating paramPath) seek
	"check for problems"]

seek :: [CommandSeek]
seek = [withAttrFilesInGit "annex.numcopies" start]

{- Checks a file's backend data for problems. -}
start :: CommandStartAttrFile
start (file, attr) = isAnnexed file $ \(key, backend) -> do
	showStart "fsck" file
	return $ Just $ perform key file backend numcopies
	where
		numcopies = readMaybe attr :: Maybe Int

perform :: Key -> FilePath -> Backend Annex -> Maybe Int -> CommandPerform
perform key file backend numcopies = do
	success <- Backend.fsckKey backend key (Just file) numcopies
	if success
		then return $ Just $ return True
		else return Nothing
