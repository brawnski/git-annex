{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Get where

import Command
import qualified Backend
import qualified Annex
import qualified Remote
import Types
import Content
import Messages
import qualified Command.Move

command :: [Command]
command = [repoCommand "get" paramPath seek
		"make content of annexed files available"]

seek :: [CommandSeek]
seek = [withFilesInGit start]

start :: CommandStartString
start file = isAnnexed file $ \(key, backend) -> do
	inannex <- inAnnex key
	if inannex
		then stop
		else do
			showStart "get" file
			from <- Annex.getState Annex.fromremote
			case from of
				Nothing -> next $ perform key backend
				Just name -> do
					src <- Remote.byName name
					next $ Command.Move.fromPerform src False key

perform :: Key -> Backend Annex -> CommandPerform
perform key backend = do
	ok <- getViaTmp key (Backend.retrieveKeyFile backend key)
	if ok
		then next $ return True -- no cleanup needed
		else stop
