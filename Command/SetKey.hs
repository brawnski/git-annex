{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.SetKey where

import Control.Monad.State (liftIO)

import Command
import Utility
import LocationLog
import Content
import Messages

command :: [Command]
command = [repoCommand "setkey" (paramRepeating paramKey) seek
	"sets annexed content for a key using a temp file"]

seek :: [CommandSeek]
seek = [withTempFile start]

{- Sets cached content for a key. -}
start :: CommandStartString
start file = do
	showStart "setkey" file
	return $ Just $ perform file

perform :: FilePath -> CommandPerform
perform file = do
	key <- cmdlineKey
	-- the file might be on a different filesystem, so mv is used
	-- rather than simply calling moveToObjectDir
	ok <- getViaTmp key $ \dest -> do
		if dest /= file
			then liftIO $
				boolSystem "mv" [File file, File dest]
			else return True
	if ok
		then return $ Just $ cleanup
		else error "mv failed!"

cleanup :: CommandCleanup
cleanup = do
	key <- cmdlineKey
	logStatus key ValuePresent
	return True
