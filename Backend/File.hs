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
import Data.List

import BackendTypes
import LocationLog
import Locations
import qualified Remotes
import qualified GitRepo as Git
import Content
import qualified Annex
import Types
import UUID
import Messages
import Trust

backend :: Backend Annex
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
	(remotes, _) <- Remotes.keyPossibilities key
	if null remotes
		then do
			showNote "not available"
			showLocations key []
			return False
		else trycopy remotes remotes
	where
		trycopy full [] = do
			showTriedRemotes full
			showLocations key []
			return False
		trycopy full (r:rs) = do
			probablythere <- probablyPresent r
			if probablythere
				then docopy r (trycopy full rs)
				else trycopy full rs
		-- This check is to avoid an ugly message if a remote is a
		-- drive that is not mounted. Avoid checking inAnnex for ssh
		-- remotes because that is unnecessarily slow, and the
		-- locationlog should be trusted. (If the ssh remote is down
		-- or really lacks the file, it's ok to show an ugly message
		-- before going on to the next remote.)
		probablyPresent r =
			if not $ Git.repoIsUrl r
				then liftIO $ doesFileExist $ gitAnnexLocation r key
				else return True
		docopy r continue = do
			showNote $ "copying from " ++ Git.repoDescribe r ++ "..."
			copied <- Remotes.copyFromRemote r key file
			if copied
				then return True
				else continue

{- Checks remotes to verify that enough copies of a key exist to allow
 - for a key to be safely removed (with no data loss), and fails with an
 - error if not. -}
checkRemoveKey :: Key -> Maybe Int -> Annex Bool
checkRemoveKey key numcopiesM = do
	force <- Annex.getState Annex.force
	if force || numcopiesM == Just 0
		then return True
		else do
			(remotes, trusteduuids) <- Remotes.keyPossibilities key
			untrusteduuids <- trustGet UnTrusted
			tocheck <- reposWithoutUUID remotes (trusteduuids++untrusteduuids)
			numcopies <- getNumCopies numcopiesM
			findcopies numcopies trusteduuids tocheck []
	where
		findcopies need have [] bad
			| length have >= need = return True
			| otherwise = notEnoughCopies need have bad
		findcopies need have (r:rs) bad
			| length have >= need = return True
			| otherwise = do
				u <- getUUID r
				let dup = elem u have
				haskey <- Remotes.inAnnex r key
				case (dup, haskey) of
					(False, Right True)	-> findcopies need (u:have) rs bad
					(False, Left _)		-> findcopies need have rs (r:bad)
					_			-> findcopies need have rs bad
		notEnoughCopies need have bad = do
			unsafe
			showLongNote $
				"Could only verify the existence of " ++
				show (length have) ++ " out of " ++ show need ++ 
				" necessary copies"
			showTriedRemotes bad
			showLocations key have
			hint
			return False
		unsafe = showNote "unsafe"
		hint = showLongNote "(Use --force to override this check, or adjust annex.numcopies.)"

showLocations :: Key -> [UUID] -> Annex ()
showLocations key exclude = do
	g <- Annex.gitRepo
	u <- getUUID g
	uuids <- liftIO $ keyLocations g key
	untrusteduuids <- trustGet UnTrusted
	let uuidswanted = filteruuids uuids (u:exclude++untrusteduuids) 
	let uuidsskipped = filteruuids uuids (u:exclude++uuidswanted)
	ppuuidswanted <- prettyPrintUUIDs uuidswanted
	ppuuidsskipped <- prettyPrintUUIDs uuidsskipped
	showLongNote $ message ppuuidswanted ppuuidsskipped
	where
		filteruuids list x = filter (\l -> not $ elem l x) list
		message [] [] = "No other repository is known to contain the file."
		message rs [] = "Try making some of these repositories available:\n" ++ rs
		message [] us = "Also these untrusted repositories may contain the file:\n" ++ us
		message rs us = message rs [] ++ message [] us

showTriedRemotes :: [Git.Repo] -> Annex ()
showTriedRemotes [] = return ()	
showTriedRemotes remotes =
	showLongNote $ "Unable to access these remotes: " ++
		Remotes.list remotes

getNumCopies :: Maybe Int -> Annex Int
getNumCopies (Just n) = return n
getNumCopies Nothing = do
	g <- Annex.gitRepo
	return $ read $ Git.configGet g config "1"
	where
		config = "annex.numcopies"

{- This is used to check that numcopies is satisfied for the key on fsck.
 - This trusts data in the the location log, and so can check all keys, even
 - those with data not present in the current annex.
 -
 - The passed action is first run to allow backends deriving this one
 - to do their own checks.
 -}
checkKey :: (Key -> Annex Bool) -> Key -> Maybe FilePath -> Maybe Int -> Annex Bool
checkKey a key file numcopies = do
	a_ok <- a key
	copies_ok <- checkKeyNumCopies key file numcopies
	return $ a_ok && copies_ok

checkKeyNumCopies :: Key -> Maybe FilePath -> Maybe Int -> Annex Bool
checkKeyNumCopies key file numcopies = do
	needed <- getNumCopies numcopies
	g <- Annex.gitRepo
	locations <- liftIO $ keyLocations g key
	untrusted <- trustGet UnTrusted
	let untrustedlocations = intersect untrusted locations
	let safelocations = filter (\l -> not $ l `elem` untrusted) locations
	let present = length safelocations
	if present < needed
		then do
			ppuuids <- prettyPrintUUIDs untrustedlocations
			warning $ missingNote (filename file key) present needed ppuuids
			return False
		else return True
	where
		filename Nothing k = show k
		filename (Just f) _ = f

missingNote :: String -> Int -> Int -> String -> String
missingNote file 0 _ [] = 
		"** No known copies of " ++ file ++ " exist!"
missingNote file 0 _ untrusted =
		"Only these untrusted locations may have copies of " ++ file ++
		"\n" ++ untrusted ++
		"Back it up to trusted locations with git-annex copy."
missingNote file present needed [] =
		"Only " ++ show present ++ " of " ++ show needed ++ 
		" trustworthy copies of " ++ file ++ " exist." ++
		"\nBack it up with git-annex copy."
missingNote file present needed untrusted = 
		missingNote file present needed [] ++
		"\nThe following untrusted locations may also have copies: " ++
		"\n" ++ untrusted
