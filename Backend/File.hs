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
import System.Directory
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
	remotes <- Remotes.keyPossibilities key
	if (null remotes)
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
			probablythere <- probablyPresent r
			if (probablythere)
				then do
					showNote $ "copying from " ++ (Git.repoDescribe r) ++ "..."
					copied <- Remotes.copyFromRemote r key file
					if (copied)
						then return True
						else trycopy full rs
				else trycopy full rs
		probablyPresent r = do
			-- This check is to avoid an ugly message if a
			-- remote is a drive that is not mounted.
			-- Avoid checking inAnnex for ssh remotes because
			-- that is unnecessarily slow, and the locationlog
			-- should be trusted. (If the ssh remote is down
			-- or really lacks the file, it's ok to show
			-- an ugly message before going on to the next
			-- remote.)
			if (not $ Git.repoIsUrl r)
				then liftIO $ doesFileExist $ annexLocation r key
				else return True

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
			remotes <- Remotes.keyPossibilities key
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
			if (have >= need)
				then return True
				else do
					haskey <- Remotes.inAnnex r key
					case (haskey) of
						Right True	-> findcopies need (have+1) rs bad
						Right False	-> findcopies need have rs bad
						Left _		-> findcopies need have rs (r:bad)
		notEnoughCopies need have bad = do
			unsafe
			showLongNote $
				"Could only verify the existence of " ++
				(show have) ++ " out of " ++ (show need) ++ 
				" necessary copies"
			if (not $ null bad) then showTriedRemotes bad else return ()
			showLocations key
			hint
			return False
		unsafe = showNote "unsafe"
		hint = showLongNote $ "(Use --force to override this check, or adjust annex.numcopies.)"

showLocations :: Key -> Annex ()
showLocations key = do
	g <- Annex.gitRepo
	u <- getUUID g
	uuids <- liftIO $ keyLocations g key
	let uuidsf = filter (\v -> v /= u) uuids
	ppuuids <- prettyPrintUUIDs uuidsf
	if (null uuidsf)
		then showLongNote $ "No other repository is known to contain the file."
		else showLongNote $ "Try making some of these repositories available:\n" ++ ppuuids
		
showTriedRemotes remotes =
	showLongNote $ "I was unable to access these remotes: " ++
		(Remotes.list remotes)
