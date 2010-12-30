{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.DropKey where

import Command
import qualified Annex
import qualified Backend
import LocationLog
import Types
import Core
import Messages

seek :: [CommandSeek]
seek = [withKeys start]

{- Drops cached content for a key. -}
start :: CommandStartString
start keyname = do
	backends <- Backend.list
	let key = genKey (head backends) keyname
	present <- inAnnex key
	force <- Annex.flagIsSet "force"
	if not present
		then return Nothing
		else if not force
			then error "dropkey is can cause data loss; use --force if you're sure you want to do this"
			else do
				showStart "dropkey" keyname
				return $ Just $ perform key

perform :: Key -> CommandPerform
perform key = do
	removeAnnex key
	return $ Just $ cleanup key

cleanup :: Key -> CommandCleanup
cleanup key = do
	logStatus key ValueMissing
	return True

