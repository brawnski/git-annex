{- git-annex pseudo-backend
 -
 - This backend does not really do any independant data storage,
 - it relies on the file contents in .git/annex/ in this repo,
 - and other accessible repos.
 -
 - This is an abstract backend; name, getKey and fsckKey have to be implemented
 - to complete it.
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Backend.File (backend, checkKey) where

import Control.Monad.State
import System.Directory

import TypeInternals
import LocationLog
import Locations
import qualified Remotes
import qualified GitRepo as Git
import Core
import qualified Annex
import UUID
import Messages

backend :: Backend
backend = Backend {
	name = mustProvide,
	getKey = mustProvide,
	storeFileKey = dummyStore,
	retrieveKeyFile = copyKeyFile,
	removeKey = checkRemoveKey,
	hasKey = inAnnex,
	fsckKey = mustProvide
}

mustProvide :: a
mustProvide = error "must provide this field"

{- Storing a key is a no-op. -}
dummyStore :: FilePath -> Key -> Annex Bool
dummyStore _ _ = return True

{- Try to find a copy of the file in one of the remotes,
 - and copy it over to this one. -}
copyKeyFile :: Key -> FilePath -> Annex Bool
copyKeyFile key file = do
	remotes <- Remotes.keyPossibilities key
	if null remotes
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
			if probablythere
				then do
					showNote $ "copying from " ++ Git.repoDescribe r ++ "..."
					copied <- Remotes.copyFromRemote r key file
					if copied
						then return True
						else trycopy full rs
				else trycopy full rs
		-- This check is to avoid an ugly message if a remote is a
		-- drive that is not mounted. Avoid checking inAnnex for ssh
		-- remotes because that is unnecessarily slow, and the
		-- locationlog should be trusted. (If the ssh remote is down
		-- or really lacks the file, it's ok to show an ugly message
		-- before going on to the next remote.)
		probablyPresent r =
			if not $ Git.repoIsUrl r
				then liftIO $ doesFileExist $ annexLocation r key
				else return True

{- Checks remotes to verify that enough copies of a key exist to allow
 - for a key to be safely removed (with no data loss), and fails with an
 - error if not. -}
checkRemoveKey :: Key -> Maybe Int -> Annex Bool
checkRemoveKey key numcopiesM = do
	force <- Annex.flagIsSet "force"
	if force || numcopiesM == Just 0
		then return True
		else do
			remotes <- Remotes.keyPossibilities key
			numcopies <- getNumCopies numcopiesM
			if numcopies > length remotes
				then notEnoughCopies numcopies (length remotes) []
				else findcopies numcopies 0 remotes []
	where
		findcopies need have [] bad
			| have >= need = return True
			| otherwise = notEnoughCopies need have bad
		findcopies need have (r:rs) bad
			| have >= need = return True
			| otherwise = do 
				haskey <- Remotes.inAnnex r key
				case haskey of
					Right True	-> findcopies need (have+1) rs bad
					Right False	-> findcopies need have rs bad
					Left _		-> findcopies need have rs (r:bad)
		notEnoughCopies need have bad = do
			unsafe
			showLongNote $
				"Could only verify the existence of " ++
				show have ++ " out of " ++ show need ++ 
				" necessary copies"
			showTriedRemotes bad
			showLocations key
			hint
			return False
		unsafe = showNote "unsafe"
		hint = showLongNote "(Use --force to override this check, or adjust annex.numcopies.)"

showLocations :: Key -> Annex ()
showLocations key = do
	g <- Annex.gitRepo
	u <- getUUID g
	uuids <- liftIO $ keyLocations g key
	let uuidsf = filter (/= u) uuids
	ppuuids <- prettyPrintUUIDs uuidsf
	if null uuidsf
		then showLongNote $ "No other repository is known to contain the file."
		else showLongNote $ "Try making some of these repositories available:\n" ++ ppuuids

showTriedRemotes :: [Git.Repo] -> Annex ()
showTriedRemotes [] = return ()	
showTriedRemotes remotes =
	showLongNote $ "I was unable to access these remotes: " ++
		Remotes.list remotes

getNumCopies :: Maybe Int -> Annex Int
getNumCopies (Just n) = return n
getNumCopies Nothing = do
	g <- Annex.gitRepo
	return $ read $ Git.configGet g config "1"
	where
		config = "annex.numcopies"

{- This is used to check that numcopies is satisfied for the key on fsck.
 - This trusts the location log, and so checks all keys, even those with
 - data not present in the current annex.
 -
 - The passed action is first run to allow backends deriving this one
 - to do their own checks.
 -}
checkKey :: (Key -> Annex Bool) -> Key -> Maybe Int -> Annex Bool
checkKey a key numcopies = do
	a_ok <- a key
	copies_ok <- checkKeyNumCopies key numcopies
	return $ a_ok && copies_ok

checkKeyNumCopies :: Key -> Maybe Int -> Annex Bool
checkKeyNumCopies key numcopies = do
	needed <- getNumCopies numcopies
	remotes <- Remotes.keyPossibilities key
	inannex <- inAnnex key
	let present = length remotes + if inannex then 1 else 0
	if present < needed
		then do
			warning $ note present needed
			return False
		else return True
	where
		note 0 _ = "** No known copies of the file exist!"
		note present needed = 
			"Only " ++ show present ++ " of " ++ show needed ++ 
			" copies of "++show key++" exist. " ++
			"Run git annex get somewhere else to back it up."
