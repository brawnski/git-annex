{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Move where

import Control.Monad.State (liftIO)

import Command
import qualified Command.Drop
import qualified Annex
import LocationLog
import Types
import Content
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
	to <- Annex.getState Annex.toremote
	from <- Annex.getState Annex.fromremote
	case (from, to) of
		(Nothing, Nothing) -> error "specify either --from or --to"
		(Nothing, Just name) -> do
			dest <- Remotes.byName name
			toStart dest move file
		(Just name, Nothing) -> do
			src <- Remotes.byName name
			fromStart src move file
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
toStart :: Git.Repo -> Bool -> CommandStartString
toStart dest move file = isAnnexed file $ \(key, _) -> do
	g <- Annex.gitRepo
	ishere <- inAnnex key
	if not ishere || g == dest
		then return Nothing -- not here, so nothing to do
		else do
			showAction move file
			return $ Just $ toPerform dest move key
toPerform :: Git.Repo -> Bool -> Key -> CommandPerform
toPerform dest move key = do
	Remotes.readConfigs
	-- checking the remote is expensive, so not done in the start step
	isthere <- Remotes.inAnnex dest key
	case isthere of
		Left err -> do
			showNote $ show err
			return Nothing
		Right False -> do
			showNote $ "to " ++ Git.repoDescribe dest ++ "..."
			ok <- Remotes.copyToRemote dest key
			if ok
				then return $ Just $ toCleanup dest move key
				else return Nothing -- failed
		Right True -> return $ Just $ toCleanup dest move key
toCleanup :: Git.Repo -> Bool -> Key -> CommandCleanup
toCleanup dest move key = do
	remoteHasKey dest key True
	if move
		then Command.Drop.cleanup key
		else return True

{- Moves (or copies) the content of an annexed file from another repository
 - to the current repository and updates locationlog information on both.
 -
 - If the current repository already has the content, it is still removed
 - from the other repository when moving.
 -}
fromStart :: Git.Repo -> Bool -> CommandStartString
fromStart src move file = isAnnexed file $ \(key, _) -> do
	g <- Annex.gitRepo
	(trusted, untrusted, _) <- Remotes.keyPossibilities key
	if (g == src) || (null $ filter (\r -> Remotes.same r src) (trusted ++ untrusted))
		then return Nothing
		else do
			showAction move file
			return $ Just $ fromPerform src move key
fromPerform :: Git.Repo -> Bool -> Key -> CommandPerform
fromPerform src move key = do
	ishere <- inAnnex key
	if ishere
		then return $ Just $ fromCleanup src move key
		else do
			showNote $ "from " ++ Git.repoDescribe src ++ "..."
			ok <- getViaTmp key $ Remotes.copyFromRemote src key
			if ok
				then return $ Just $ fromCleanup src move key
				else return Nothing -- fail
fromCleanup :: Git.Repo -> Bool -> Key -> CommandCleanup
fromCleanup src True key = do
	ok <- Remotes.onRemote src (boolSystem, False) "dropkey" 
		["--quiet", "--force",
		"--backend=" ++ backendName key,
		keyName key]
	-- better safe than sorry: assume the src dropped the key
	-- even if it seemed to fail; the failure could have occurred
	-- after it really dropped it
	remoteHasKey src key False
	return ok
fromCleanup _ False _ = return True
