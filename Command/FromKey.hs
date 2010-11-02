{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.FromKey where

import Control.Monad.State (liftIO)
import System.Posix.Files
import System.Directory
import Control.Monad (when, unless)

import Command
import qualified Annex
import Utility
import qualified Backend
import Types
import Core

{- Adds a file pointing at a manually-specified key -}
start :: SubCmdStartString
start file = do
	keyname <- Annex.flagGet "key"
	when (null keyname) $ error "please specify the key with --key"
	backends <- Backend.list
	let key = genKey (backends !! 0) keyname

	inbackend <- Backend.hasKey key
	unless (inbackend) $ error $
		"key ("++keyname++") is not present in backend"
	showStart "fromkey" file
	return $ Just $ perform file key
perform :: FilePath -> Key -> SubCmdPerform
perform file key = do
	link <- calcGitLink file key
	liftIO $ createDirectoryIfMissing True (parentDir file)
	liftIO $ createSymbolicLink link file
	return $ Just $ cleanup file
cleanup :: FilePath -> SubCmdCleanup
cleanup file = do
	Annex.queue "add" [] file
	return True
