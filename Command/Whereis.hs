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
import Messages
import UUID
import Types

command :: [Command]
command = [repoCommand "whereis" (paramOptional $ paramRepeating paramPath) seek
	"lists repositories that have file content"]

seek :: [CommandSeek]
seek = [withFilesInGit start]

start :: CommandStartString
start file = isAnnexed file $ \(key, _) -> do
	showStart "whereis" file
	next $ perform key

perform :: Key -> CommandPerform
perform key = do
	g <- Annex.gitRepo
	uuids <- liftIO $ keyLocations g key
	let num = length uuids
	showNote $ show num ++ " " ++ copiesplural num
	if null $ uuids
		then stop
		else do
			pp <- prettyPrintUUIDs uuids
			showLongNote $ pp
			showProgress	
			next $ return True
	where
		copiesplural 1 = "copy"
		copiesplural _ = "copies"
