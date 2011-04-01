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
import qualified Remote
import UUID
import Messages
import Utility

command :: [Command]
command = [repoCommand "move" paramPath seek
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
			dest <- Remote.byName name
			toStart dest move file
		(Just name, Nothing) -> do
			src <- Remote.byName name
			fromStart src move file
		(_ ,  _) -> error "only one of --from or --to can be specified"

showAction :: Bool -> FilePath -> Annex ()
showAction True file = showStart "move" file
showAction False file = showStart "copy" file

{- Used to log a change in a remote's having a key. The change is logged
 - in the local repo, not on the remote. The process of transferring the
 - key to the remote, or removing the key from it *may* log the change
 - on the remote, but this cannot be relied on. For example, it's not done
 - for bare repos. -}
remoteHasKey :: Remote.Remote Annex -> Key -> Bool -> Annex ()
remoteHasKey remote key present	= do
	g <- Annex.gitRepo
	let remoteuuid = Remote.uuid remote
	logfile <- liftIO $ logChange g key remoteuuid status
	Annex.queue "add" [Param "--"] logfile
	where
		status = if present then ValuePresent else ValueMissing

{- Moves (or copies) the content of an annexed file to a remote.
 -
 - If the remote already has the content, it is still removed from
 - the current repository.
 -
 - Note that unlike drop, this does not honor annex.numcopies.
 - A file's content can be moved even if there are insufficient copies to
 - allow it to be dropped.
 -}
toStart :: Remote.Remote Annex -> Bool -> CommandStartString
toStart dest move file = isAnnexed file $ \(key, _) -> do
	g <- Annex.gitRepo
	u <- getUUID g
	ishere <- inAnnex key
	if not ishere || u == Remote.uuid dest
		then return Nothing -- not here, so nothing to do
		else do
			showAction move file
			return $ Just $ toPerform dest move key
toPerform :: Remote.Remote Annex -> Bool -> Key -> CommandPerform
toPerform dest move key = do
	-- Checking the remote is expensive, so not done in the start step.
	-- In fast mode, location tracking is assumed to be correct,
	-- and an explicit check is not done, when copying. When moving,
	-- it has to be done, to avoid inaverdent data loss.
	fast <- Annex.getState Annex.fast
	isthere <- if fast && not move && not (Remote.hasKeyCheap dest)
		then do
			(remotes, _) <- Remote.keyPossibilities key
			return $ Right $ dest `elem` remotes
		else Remote.hasKey dest key
	case isthere of
		Left err -> do
			showNote $ show err
			return Nothing
		Right False -> do
			showNote $ "to " ++ Remote.name dest ++ "..."
			ok <- Remote.storeKey dest key
			if ok
				then return $ Just $ toCleanup dest move key
				else return Nothing -- failed
		Right True -> return $ Just $ toCleanup dest move key
toCleanup :: Remote.Remote Annex -> Bool -> Key -> CommandCleanup
toCleanup dest move key = do
	remoteHasKey dest key True
	if move
		then Command.Drop.cleanup key
		else return True

{- Moves (or copies) the content of an annexed file from a remote
 - to the current repository.
 -
 - If the current repository already has the content, it is still removed
 - from the remote.
 -}
fromStart :: Remote.Remote Annex -> Bool -> CommandStartString
fromStart src move file = isAnnexed file $ \(key, _) -> do
	g <- Annex.gitRepo
	u <- getUUID g
	(remotes, _) <- Remote.keyPossibilities key
	if (u == Remote.uuid src) || (null $ filter (== src) remotes)
		then return Nothing
		else do
			showAction move file
			return $ Just $ fromPerform src move key
fromPerform :: Remote.Remote Annex -> Bool -> Key -> CommandPerform
fromPerform src move key = do
	ishere <- inAnnex key
	if ishere
		then return $ Just $ fromCleanup src move key
		else do
			showNote $ "from " ++ Remote.name src ++ "..."
			ok <- getViaTmp key $ Remote.retrieveKeyFile src key
			if ok
				then return $ Just $ fromCleanup src move key
				else return Nothing -- fail
fromCleanup :: Remote.Remote Annex -> Bool -> Key -> CommandCleanup
fromCleanup src True key = do
	ok <- Remote.removeKey src key
	-- better safe than sorry: assume the src dropped the key
	-- even if it seemed to fail; the failure could have occurred
	-- after it really dropped it
	remoteHasKey src key False
	return ok
fromCleanup _ False _ = return True
