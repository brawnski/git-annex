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
import Locations
import qualified Backend
import LocationLog
import Types
import Core

{- Sets cached content for a key. -}
start :: SubCmdStartString
start tmpfile = do
	keyname <- Annex.flagGet "key"
	when (null keyname) $ error "please specify the key with --key"
	backends <- Backend.list
	let key = genKey (backends !! 0) keyname
	showStart "setkey" tmpfile
	return $ Just $ perform tmpfile key
perform :: FilePath -> Key -> SubCmdPerform
perform tmpfile key = do
	g <- Annex.gitRepo
	let loc = annexLocation g key
	ok <- liftIO $ boolSystem "mv" [tmpfile, loc]
	if (not ok)
		then error "mv failed!"
		else return $ Just $ cleanup key
cleanup :: Key -> SubCmdCleanup
cleanup key = do
	logStatus key ValuePresent
	return True

