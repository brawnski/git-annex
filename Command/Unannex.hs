{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Unannex where

import Control.Monad.State (liftIO)
import System.Directory

import Command
import qualified Annex
import Utility
import Locations
import qualified Backend
import LocationLog
import Types
import Core
import qualified GitRepo as Git

{- The unannex subcommand undoes an add. -}
start :: SubCmdStartString
start file = isAnnexed file $ \(key, backend) -> do
	showStart "unannex" file
	return $ Just $ perform file key backend

perform :: FilePath -> Key -> Backend -> SubCmdPerform
perform file key backend = do
	-- force backend to always remove
	Annex.flagChange "force" $ FlagBool True
	ok <- Backend.removeKey backend key
	if (ok)
		then return $ Just $ cleanup file key
		else return Nothing

cleanup :: FilePath -> Key -> SubCmdCleanup
cleanup file key = do
	logStatus key ValueMissing
	g <- Annex.gitRepo
	let src = annexLocation g key
	liftIO $ removeFile file
	liftIO $ Git.run g ["rm", "--quiet", file]
	-- git rm deletes empty directories; put them back
	liftIO $ createDirectoryIfMissing True (parentDir file)
	liftIO $ renameFile src file
	return True