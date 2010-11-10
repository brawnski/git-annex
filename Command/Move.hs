{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Move where

import Control.Monad.State (liftIO)
import Monad (when)

import Command
import Command.Drop
import qualified Annex
import Locations
import LocationLog
import Types
import Core
import qualified GitRepo as Git
import qualified Remotes
import UUID
import Messages

{- Move a file either --to or --from a repository.
 -
 - This only operates on the cached file content; it does not involve
 - moving data in the key-value backend. -}
start :: SubCmdStartString
start file = do
	fromName <- Annex.flagGet "fromrepository"
	toName <- Annex.flagGet "torepository"
	case (fromName, toName) of
		("", "") -> error "specify either --from or --to"
		("",  _) -> moveToStart file
		(_ , "") -> moveFromStart file
		(_ ,  _) -> error "only one of --from or --to can be specified"

{- Moves the content of an annexed file to another repository,
 - removing it from the current repository, and updates locationlog
 - information on both.
 -
 - If the destination already has the content, it is still removed
 - from the current repository.
 -
 - Note that unlike drop, this does not honor annex.numcopies.
 - A file's content can be moved even if there are insufficient copies to
 - allow it to be dropped.
 -}
moveToStart :: SubCmdStartString
moveToStart file = isAnnexed file $ \(key, _) -> do
	ishere <- inAnnex key
	if (not ishere)
		then return Nothing -- not here, so nothing to do
		else do
			showStart "move" file
			return $ Just $ moveToPerform key
moveToPerform :: Key -> SubCmdPerform
moveToPerform key = do
	-- checking the remote is expensive, so not done in the start step
	remote <- Remotes.commandLineRemote
	isthere <- Remotes.inAnnex remote key
	case isthere of
		Left err -> do
			showNote $ show err
			return Nothing
		Right False -> do
			showNote $ "moving to " ++ (Git.repoDescribe remote) ++ "..."
			let tmpfile = (annexTmpLocation remote) ++ (keyFile key)
			ok <- Remotes.copyToRemote remote key tmpfile
			if (ok)
				then return $ Just $ moveToCleanup remote key tmpfile
				else return Nothing -- failed
		Right True -> return $ Just $ Command.Drop.cleanup key
moveToCleanup :: Git.Repo -> Key -> FilePath -> SubCmdCleanup
moveToCleanup remote key tmpfile = do
	-- Tell remote to use the transferred content.
	ok <- Remotes.runCmd remote "git-annex" ["setkey", "--quiet",
		"--backend=" ++ (backendName key),
		"--key=" ++ keyName key,
		tmpfile]
	if ok
		then do
			-- Record that the key is present on the remote.
			g <- Annex.gitRepo
			remoteuuid <- getUUID remote
			logfile <- liftIO $ logChange g key remoteuuid ValuePresent
			Annex.queue "add" ["--"] logfile
			-- Cleanup on the local side is the same as done for the
			-- drop subcommand.
			Command.Drop.cleanup key
		else return False

{- Moves the content of an annexed file from another repository to the current
 - repository and updates locationlog information on both.
 -
 - If the current repository already has the content, it is still removed
 - from the other repository.
 -}
moveFromStart :: SubCmdStartString
moveFromStart file = isAnnexed file $ \(key, _) -> do
	remote <- Remotes.commandLineRemote
	l <- Remotes.keyPossibilities key
	if (null $ filter (\r -> Remotes.same r remote) l)
		then return Nothing
		else do
			showStart "move" file
			return $ Just $ moveFromPerform key
moveFromPerform :: Key -> SubCmdPerform
moveFromPerform key = do
	remote <- Remotes.commandLineRemote
	ishere <- inAnnex key
	if (ishere)
		then return $ Just $ moveFromCleanup remote key
		else do
			showNote $ "moving from " ++ (Git.repoDescribe remote) ++ "..."
			ok <- getViaTmp key (Remotes.copyFromRemote remote key)
			if (ok)
				then return $ Just $ moveFromCleanup remote key
				else return Nothing -- fail
moveFromCleanup :: Git.Repo -> Key -> SubCmdCleanup
moveFromCleanup remote key = do
	ok <- Remotes.runCmd remote "git-annex" ["dropkey", "--quiet", "--force",
		"--backend=" ++ (backendName key),
		keyName key]
	when ok $ do
		-- Record locally that the key is not on the remote.
		remoteuuid <- getUUID remote
		g <- Annex.gitRepo
		logfile <- liftIO $ logChange g key remoteuuid ValueMissing
		Annex.queue "add" ["--"] logfile
	return ok
