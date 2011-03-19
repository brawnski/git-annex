{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Fsck where

import Control.Monad.State (liftIO)

import Command
import qualified Backend
import qualified Annex
import UUID
import Types
import Messages
import Utility
import Content
import LocationLog

command :: [Command]
command = [repoCommand "fsck" (paramOptional $ paramRepeating paramPath) seek
	"check for problems"]

seek :: [CommandSeek]
seek = [withAttrFilesInGit "annex.numcopies" start]

start :: CommandStartAttrFile
start (file, attr) = notBareRepo $ isAnnexed file $ \(key, backend) -> do
	showStart "fsck" file
	return $ Just $ perform key file backend numcopies
	where
		numcopies = readMaybe attr :: Maybe Int

perform :: Key -> FilePath -> Backend Annex -> Maybe Int -> CommandPerform
perform key file backend numcopies = do
	-- the location log is checked first, so that if it has bad data
	-- that gets corrected
	locationlogok <- verifyLocationLog key file
	backendok <- Backend.fsckKey backend key (Just file) numcopies
	if locationlogok && backendok
		then return $ Just $ return True
		else return Nothing

{- Checks that the location log reflects the current status of the key,
   in this repository only. -}
verifyLocationLog :: Key -> FilePath -> Annex Bool
verifyLocationLog key file = do
	present <- inAnnex key
	
	g <- Annex.gitRepo
	u <- getUUID g
        uuids <- liftIO $ keyLocations g key

	case (present, u `elem` uuids) of
		(True, False) -> do
				fix g u ValuePresent
				-- There is no data loss, so do not fail.
				return True
		(False, True) -> do
				fix g u ValueMissing
				warning $
					"** Based on the location log, " ++ file
					++ "\n** was expected to be present, " ++
					"but its content is missing."
				return False
		_ -> return True
	
	where
		fix g u s = do
			showNote "fixing location log"
			_ <- liftIO $ logChange g key u s
			return ()
