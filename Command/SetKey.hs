{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.SetKey where

import Control.Monad.State (liftIO)
import Control.Monad (when)

import Command
import qualified Annex
import Utility
import qualified Backend
import LocationLog
import Types
import Core
import Messages

seek :: [SubCmdSeek]
seek = [withTempFile start]

{- Sets cached content for a key. -}
start :: SubCmdStartString
start file = do
	keyname <- Annex.flagGet "key"
	when (null keyname) $ error "please specify the key with --key"
	backends <- Backend.list
	let key = genKey (head backends) keyname
	showStart "setkey" file
	return $ Just $ perform file key
perform :: FilePath -> Key -> SubCmdPerform
perform file key = do
	-- the file might be on a different filesystem, so mv is used
	-- rather than simply calling moveToObjectDir key file
	ok <- getViaTmp key $ \dest -> do
		if dest /= file
			then liftIO $ boolSystem "mv" [file, dest]
			else return True
	if ok
		then return $ Just $ cleanup key
		else error "mv failed!"

cleanup :: Key -> SubCmdCleanup
cleanup key = do
	logStatus key ValuePresent
	return True

