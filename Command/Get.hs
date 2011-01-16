{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Get where

import Command
import qualified Backend
import Types
import Content
import Messages

command :: [Command]
command = [Command "get" paramPath seek
		"make content of annexed files available"]

seek :: [CommandSeek]
seek = [withFilesInGit start]

{- Gets an annexed file from one of the backends. -}
start :: CommandStartString
start file = isAnnexed file $ \(key, backend) -> do
	inannex <- inAnnex key
	if inannex
		then return Nothing
		else do
			showStart "get" file
			return $ Just $ perform key backend

perform :: Key -> Backend -> CommandPerform
perform key backend = do
	ok <- getViaTmp key (Backend.retrieveKeyFile backend key)
	if ok
		then return $ Just $ return True -- no cleanup needed
		else return Nothing

