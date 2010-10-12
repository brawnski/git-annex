{- git-annex toplevel code
 -}

module Annex (
	State,
	startAnnex,
	annexFile,
	unannexFile
) where

import System.Posix.Files
import System.Directory
import GitRepo
import Utility
import Locations
import Backend
import BackendList
import UUID
import LocationLog

-- git-annex's runtime state
data State = State {
	repo :: GitRepo,
	backends :: [Backend]
}

{- An annexed file's content is stored somewhere under .git/annex/ -}
annexDir :: GitRepo -> Key -> FilePath
annexDir repo key = gitDir repo ++ "/annex/" ++ key

{- On startup, examine the git repo, prepare it, and record state for
 - later. -}
startAnnex :: IO State
startAnnex = do
	r <- gitRepoFromCwd
	gitPrep r

	return State {
		repo = r,
		backends = parseBackendList $ gitConfig r "annex.backends" ""
	}

{- Annexes a file, storing it in a backend, and then moving it into
 - the annex directory and setting up the symlink pointing to its content. -}
annexFile :: State -> FilePath -> IO ()
annexFile state file = do
	alreadyannexed <- lookupBackend (backends state) (repo state) file
	case (alreadyannexed) of
		Just _ -> error $ "already annexed: " ++ file
		Nothing -> do
			checkLegal file
			stored <- storeFile (backends state) (repo state) file
			case (stored) of
				Nothing -> error $ "no backend could store: " ++ file
				Just key -> symlink key
	where
		symlink key = do
			let dest = annexDir (repo state) key
			createDirectoryIfMissing True (parentDir dest)
			renameFile file dest
			createSymbolicLink dest file
			gitAdd (repo state) file
		checkLegal file = do
			s <- getSymbolicLinkStatus file
			if ((isSymbolicLink s) || (not $ isRegularFile s))
				then error $ "not a regular file: " ++ file
				else return ()

{- Inverse of annexFile. -}
unannexFile :: State -> FilePath -> IO ()
unannexFile state file = do
	alreadyannexed <- lookupBackend (backends state) (repo state) file
	case (alreadyannexed) of
		Nothing -> error $ "not annexed " ++ file
		Just _ -> do
			mkey <- dropFile (backends state) (repo state) file
			case (mkey) of
				Nothing -> return ()
				Just key -> do
					let src = annexDir (repo state) key
					removeFile file
					renameFile src file
					return ()

{- Sets up a git repo for git-annex. May be called repeatedly. -}
gitPrep :: GitRepo -> IO ()
gitPrep repo = do
	prepUUID repo

	-- configure git to use union merge driver on state files
	let attrLine = stateLoc ++ "/*.log merge=union"
	let attributes = gitAttributes repo
	exists <- doesFileExist attributes
	if (not exists)
		then do
			writeFile attributes $ attrLine ++ "\n"
			gitAdd repo attributes
		else do
			content <- readFile attributes
			if (all (/= attrLine) (lines content))
				then do
					appendFile attributes $ attrLine ++ "\n"
					gitAdd repo attributes
				else return ()
