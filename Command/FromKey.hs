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
import Control.Monad (unless)

import Command
import qualified Annex
import Utility
import qualified Backend
import Types
import Content
import Messages

command :: [Command]
command = [Command "fromkey" paramPath seek
	"adds a file using a specific key"]

seek :: [CommandSeek]
seek = [withFilesMissing start]

start :: CommandStartString
start file = notBareRepo $ do
	key <- cmdlineKey
	inbackend <- Backend.hasKey key
	unless inbackend $ error $
		"key ("++keyName key++") is not present in backend"
	showStart "fromkey" file
	return $ Just $ perform file

perform :: FilePath -> CommandPerform
perform file = do
	key <- cmdlineKey
	link <- calcGitLink file key
	liftIO $ createDirectoryIfMissing True (parentDir file)
	liftIO $ createSymbolicLink link file
	return $ Just $ cleanup file

cleanup :: FilePath -> CommandCleanup
cleanup file = do
	Annex.queue "add" [Param "--"] file
	return True
