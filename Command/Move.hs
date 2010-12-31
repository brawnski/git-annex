{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Move where

import Control.Monad (when)
import Control.Monad.State (liftIO)

import Command
import qualified Command.Drop
import qualified Annex
import Locations
import LocationLog
import Types
import Core
import qualified GitRepo as Git
import qualified Remotes
import UUID
import Messages
import Utility
	
command :: [Command]
command = [Command "move" paramPath seek
	"move content of files to/from another repository"]

seek :: [CommandSeek]
seek = [withFilesInGit $ start True]

{- Move (or copy) a file either --to or --from a repository.
 -
 - This only operates on the cached file content; it does not involve
 - moving data in the key-value backend. -}
start :: Bool -> CommandStartString
start move file = do
	fromName <- Annex.flagGet "fromrepository"
	toName <- Annex.flagGet "torepository"
	case (fromName, toName) of
		("", "") -> error "specify either --from or --to"
		("",  _) -> toStart move file
		(_ , "") -> fromStart move file
		(_ ,  _) -> error "only one of --from or --to can be specified"

showAction :: Bool -> FilePath -> Annex ()
showAction True file = showStart "move" file
showAction False file = showStart "copy" file

remoteHasKey :: Git.Repo -> Key -> Bool -> Annex ()
remoteHasKey remote key present	= do
	g <- Annex.gitRepo
	remoteuuid <- getUUID remote
	logfile <- liftIO $ logChange g key remoteuuid status
	Annex.queue "add" ["--"] logfile
	where
		status = if present then ValuePresent else ValueMissing

{- Moves (or copies) the content of an annexed file to another repository,
 - and updates locationlog information on both.
 -
 - When moving, if the destination already has the content, it is
 - still removed from the current repository.
 -
 - Note that unlike drop, this does not honor annex.numcopies.
 - A file's content can be moved even if there are insufficient copies to
 - allow it to be dropped.
 -}
toStart :: Bool -> CommandStartString
toStart move file = isAnnexed file $ \(key, _) -> do
	ishere <- inAnnex key
	if not ishere
		then return Nothing -- not here, so nothing to do
		else do
			showAction move file
			return $ Just $ toPerform move key
toPerform :: Bool -> Key -> CommandPerform
toPerform move key = do
	-- checking the remote is expensive, so not done in the start step
	remote <- Remotes.commandLineRemote
	isthere <- Remotes.inAnnex remote key
	case isthere of
		Left err -> do
			showNote $ show err
			return Nothing
		Right False -> do
			showNote $ "to " ++ Git.repoDescribe remote ++ "..."
			let tmpfile = annexTmpLocation remote ++ keyFile key
			ok <- Remotes.copyToRemote remote key tmpfile
			if ok
				then return $ Just $ toCleanup move remote key tmpfile
				else return Nothing -- failed
		Right True -> return $ Just $ Command.Drop.cleanup key
toCleanup :: Bool -> Git.Repo -> Key -> FilePath -> CommandCleanup
toCleanup move remote key tmpfile = do
	-- Tell remote to use the transferred content.
	ok <- Remotes.runCmd remote "git-annex" ["setkey", "--quiet",
		"--backend=" ++ backendName key,
		"--key=" ++ keyName key,
		tmpfile]
	if ok
		then do
			remoteHasKey remote key True
			if move
				then Command.Drop.cleanup key
				else return True
		else return False

{- Moves (or copies) the content of an annexed file from another repository
 - to the current repository and updates locationlog information on both.
 -
 - If the current repository already has the content, it is still removed
 - from the other repository when moving.
 -}
fromStart :: Bool -> CommandStartString
fromStart move file = isAnnexed file $ \(key, _) -> do
	remote <- Remotes.commandLineRemote
	(trusted, untrusted, _) <- Remotes.keyPossibilities key
	if null $ filter (\r -> Remotes.same r remote) (trusted ++ untrusted)
		then return Nothing
		else do
			showAction move file
			return $ Just $ fromPerform move key
fromPerform :: Bool -> Key -> CommandPerform
fromPerform move key = do
	remote <- Remotes.commandLineRemote
	ishere <- inAnnex key
	if ishere
		then return $ Just $ fromCleanup move remote key
		else do
			showNote $ "from " ++ Git.repoDescribe remote ++ "..."
			ok <- getViaTmp key $ Remotes.copyFromRemote remote key
			if ok
				then return $ Just $ fromCleanup move remote key
				else return Nothing -- fail
fromCleanup :: Bool -> Git.Repo -> Key -> CommandCleanup
fromCleanup True remote key = do
	ok <- Remotes.onRemote remote (boolSystem, False) "dropkey" 
		["--quiet", "--force",
		"--backend=" ++ backendName key,
		keyName key]
	when ok $
		remoteHasKey remote key False
	return ok
fromCleanup False _ _ = return True
