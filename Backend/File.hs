{- git-annex pseudo-backend
 -
 - This backend does not really do any independant data storage,
 - it relies on the file contents in .git/annex/ in this repo,
 - and other accessible repos.
 -
 - This is an abstract backend; getKey has to be implemented to complete
 - it.
 -}

module Backend.File (backend) where

import Control.Monad.State
import System.IO
import System.Cmd
import System.Exit
import Control.Exception

import BackendTypes
import LocationLog
import Locations
import qualified Remotes
import qualified GitRepo as Git
import Utility
import Core
import qualified Annex
import UUID
import qualified Backend

backend = Backend {
	name = mustProvide,
	getKey = mustProvide,
	storeFileKey = dummyStore,
	retrieveKeyFile = copyKeyFile,
	removeKey = checkRemoveKey,
	hasKey = checkKeyFile
}

mustProvide = error "must provide this field"

{- Storing a key is a no-op. -}
dummyStore :: FilePath -> Key -> Annex (Bool)
dummyStore file key = return True

{- Just check if the .git/annex/ file for the key exists. -}
checkKeyFile :: Key -> Annex Bool
checkKeyFile k = inAnnex k

{- Try to find a copy of the file in one of the remotes,
 - and copy it over to this one. -}
copyKeyFile :: Key -> FilePath -> Annex (Bool)
copyKeyFile key file = do
	remotes <- Remotes.withKey key
	if (0 == length remotes)
		then cantfind
		else trycopy remotes remotes
	where
		trycopy full [] = do
			showNote $
				"need access to one of these remotes: " ++
				(Remotes.list full)
			return False
		trycopy full (r:rs) = do
			-- annexLocation needs the git config to have been
			-- read for a remote, so do that now,
			-- if it hasn't been already
			result <- Remotes.tryGitConfigRead r
			case (result) of
				Nothing -> trycopy full rs
				Just r' -> do
					showNote $ "copying from " ++ (Git.repoDescribe r ) ++ "..."
					result <- liftIO $ (try (copyFromRemote r' key file)::IO (Either SomeException ()))
		        		case (result) of
				                Left err -> do
							liftIO $ hPutStrLn stderr (show err)
							trycopy full rs
				                Right succ -> return True
		cantfind = do
			g <- Annex.gitRepo
			uuids <- liftIO $ keyLocations g key
			ppuuids <- prettyPrintUUIDs uuids
			showNote $ "No available git remotes have the file."
			if (0 < length uuids)
				then showLongNote $ "It has been seen before in these repositories:\n" ++ ppuuids
				else return ()
			return False

{- Tries to copy a file from a remote, exception on error. -}
copyFromRemote :: Git.Repo -> Key -> FilePath -> IO ()
copyFromRemote r key file = do
	if (Git.repoIsLocal r)
		then getlocal
		else getremote
	where
		getlocal = do
			res <-rawSystem "cp" ["-a", location, file]
			if (res == ExitSuccess)
				then return ()
				else error "cp failed"
		getremote = error "get via network not yet implemented!"
		location = annexLocation r key

{- Checks remotes to verify that enough copies of a key exist to allow
 - for a key to be safely removed (with no data loss), and fails with an
 - error if not. -}
checkRemoveKey :: Key -> Annex (Bool)
checkRemoveKey key = do
	force <- Annex.flagIsSet Force
	if (force)
		then return True
		else do
			g <- Annex.gitRepo
			let numcopies = read $ Git.configGet g config "1"
			remotes <- Remotes.withKey key
			if (numcopies > length remotes)
				then retNotEnoughCopiesKnown remotes numcopies
				else findcopies numcopies remotes []
	where
		config = "annex.numcopies"

		findcopies 0 _ _ = return True -- success, enough copies found
		findcopies _ [] bad = notEnoughCopiesSeen bad
		findcopies n (r:rs) bad = do
			all <- Annex.supportedBackends
			result <- liftIO $ ((try $ remoteHasKey r all)::IO (Either SomeException Bool))
			case (result) of
				Right True	-> findcopies (n-1) rs bad
				Right False	-> findcopies n rs bad
				Left _		-> findcopies n rs (r:bad)
		remoteHasKey r all = do
			-- To check if a remote has a key, construct a new
			-- Annex monad and query its backend.
			a <- Annex.new r all
			(result, _) <- Annex.run a (Backend.hasKey key)
			return result
		notEnoughCopiesSeen bad = do
			showNote "failed to find enough other copies of the file"
			if (0 /= length bad) then listbad bad else return ()
			unsafe
			return False
		listbad bad =
			showLongNote $ 
				"I was unable to access these remotes: " ++
				(Remotes.list bad)
		retNotEnoughCopiesKnown remotes numcopies = do
			showNote $
				"I only know about " ++ (show $ length remotes) ++ 
				" out of " ++ (show numcopies) ++ 
				" necessary copies of the file"
			unsafe
			return False
		unsafe = do
			showLongNote $ "According to the " ++ config ++
				" setting, it is not safe to remove it!"
			showLongNote "(Use --force to override.)"
