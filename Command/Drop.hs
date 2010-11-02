{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Drop where

import Control.Monad.State (liftIO)
import System.Directory

import Command
import qualified Annex
import Locations
import qualified Backend
import LocationLog
import Types
import Core

{- Indicates a file's content is not wanted anymore, and should be removed
 - if it's safe to do so. -}
start :: SubCmdStartString
start file = isAnnexed file $ \(key, backend) -> do
	inbackend <- Backend.hasKey key
	if (not inbackend)
		then return Nothing
		else do
			showStart "drop" file
			return $ Just $ perform key backend

perform :: Key -> Backend -> SubCmdPerform
perform key backend = do
	success <- Backend.removeKey backend key
	if (success)
		then return $ Just $ cleanup key
		else return Nothing

cleanup :: Key -> SubCmdCleanup
cleanup key = do
	logStatus key ValueMissing
	inannex <- inAnnex key
	if (inannex)
		then do
			g <- Annex.gitRepo
			let loc = annexLocation g key
			liftIO $ removeFile loc
			return True
		else return True

