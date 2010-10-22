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
import System.Cmd.Utils
import Control.Exception
import List
import Maybe

import TypeInternals
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
		then do
			showNote "not available"
			showLocations key
			return False
		else trycopy remotes remotes
	where
		trycopy full [] = do
			showNote "not available"
			showTriedRemotes full
			showLocations key
			return False
		trycopy full (r:rs) = do
			-- annexLocation needs the git config to have been
			-- read for a remote, so do that now,
			-- if it hasn't been already
			result <- Remotes.tryGitConfigRead r
			case (result) of
				Left err -> trycopy full rs
				Right r' -> do
					showNote $ "copying from " ++ (Git.repoDescribe r) ++ "..."
					liftIO $ copyFromRemote r' key file

{- Tries to copy a file from a remote. -}
copyFromRemote :: Git.Repo -> Key -> FilePath -> IO Bool
copyFromRemote r key file = do
	if (not $ Git.repoIsUrl r)
		then getlocal
		else if (Git.repoIsSsh r)
			then getssh
			else error "copying from non-ssh repo not supported"
	where
		location = annexLocation r key
		getlocal = boolSystem "cp" ["-a", location, file]
		getssh = do
			liftIO $ putStrLn "" -- make way for scp progress bar
			boolSystem "scp" [location, file]

showLocations :: Key -> Annex ()
showLocations key = do
	g <- Annex.gitRepo
	u <- getUUID g
	uuids <- liftIO $ keyLocations g key
	let uuidsf = filter (\v -> v /= u) uuids
	ppuuids <- prettyPrintUUIDs uuidsf
	if (0 < length uuidsf)
		then showLongNote $ "Try making some of these repositories available:\n" ++ ppuuids
		else showLongNote $ "No other repository is known to contain the file."
		
showTriedRemotes remotes =
	showLongNote $ "I was unable to access these remotes: " ++
		(Remotes.list remotes)

{- Checks remotes to verify that enough copies of a key exist to allow
 - for a key to be safely removed (with no data loss), and fails with an
 - error if not. -}
checkRemoveKey :: Key -> Annex (Bool)
checkRemoveKey key = do
	force <- Annex.flagIsSet "force"
	if (force)
		then return True
		else do
			g <- Annex.gitRepo
			remotes <- Remotes.withKey key
			let numcopies = read $ Git.configGet g config "1"
			if (numcopies > length remotes)
				then notEnoughCopies numcopies (length remotes) []
				else findcopies numcopies 0 remotes []
	where
		config = "annex.numcopies"
		findcopies need have [] bad = 
			if (have >= need)
				then return True
				else notEnoughCopies need have bad
		findcopies need have (r:rs) bad = do
			all <- Annex.supportedBackends
			result <- liftIO $ ((try $ remoteHasKey r all)::IO (Either SomeException Bool))
			case (result) of
				Right True	-> findcopies need (have+1) rs bad
				Right False	-> findcopies need have rs bad
				Left _		-> findcopies need have rs (r:bad)
		remoteHasKey remote all = do
			-- To check if a remote has a key, construct a new
			-- Annex monad and query its backend.
			a <- Annex.new remote all
			(result, _) <- Annex.run a (Backend.hasKey key)
			return result
		notEnoughCopies need have bad = do
			unsafe
			showLongNote $
				"Could only verify the existence of " ++
				(show have) ++ " out of " ++ (show need) ++ 
				" necessary copies"
			if (0 /= length bad) then showTriedRemotes bad else return ()
			showLocations key
			hint
			return False
		unsafe = showNote "unsafe"
		hint = showLongNote $ "(Use --force to override this check, or adjust annex.numcopies.)"
