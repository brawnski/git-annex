{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.DropKey where

import Control.Monad.State (liftIO)
import System.Directory

import Command
import qualified Annex
import Locations
import qualified Backend
import LocationLog
import Types
import Core

{- Drops cached content for a key. -}
start :: SubCmdStartString
start keyname = do
	backends <- Backend.list
	let key = genKey (backends !! 0) keyname
	present <- inAnnex key
	force <- Annex.flagIsSet "force"
	if (not present)
		then return Nothing
		else if (not force)
			then error "dropkey is can cause data loss; use --force if you're sure you want to do this"
			else do
				showStart "dropkey" keyname
				return $ Just $ perform key

perform :: Key -> SubCmdPerform
perform key = do
	g <- Annex.gitRepo
	let loc = annexLocation g key
	liftIO $ removeFile loc
	return $ Just $ cleanup key

cleanup :: Key -> SubCmdCleanup
cleanup key = do
	logStatus key ValueMissing
	return True

