{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Whereis where

import Control.Monad.State (liftIO)

import qualified Annex
import LocationLog
import Command
import Content
import Messages
import UUID
import Types

command :: [Command]
command = [Command "whereis" (paramOptional $ paramRepeating paramPath) seek
	"lists repositories that have file content"]

seek :: [CommandSeek]
seek = [withFilesInGit start]

start :: CommandStartString
start file = isAnnexed file $ \(key, _) -> do
	showStart "whereis" file
	return $ Just $ perform key

perform :: Key -> CommandPerform
perform key = do
	g <- Annex.gitRepo
	uuids <- liftIO $ keyLocations g key
	pp <- prettyPrintUUIDs uuids
	showLongNote $ pp
	showProgress
	if null $ uuids
		then return Nothing
		else return $ Just $ return True
