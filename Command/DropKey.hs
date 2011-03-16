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
import Content
import Messages

command :: [Command]
command = [Command "dropkey" (paramRepeating paramKey) seek
	"drops annexed content for specified keys"] 

seek :: [CommandSeek]
seek = [withKeys start]

{- Drops cached content for a key. -}
start :: CommandStartString
start keyname = do
	backends <- Backend.list
	let key = error "fixme!!"
	--let key = genKey (head backends) keyname --TODO FIXME
	let present  = error "fixme!!"
	--present <- inAnnex key
	force <- Annex.getState Annex.force
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

