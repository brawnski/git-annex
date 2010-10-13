{- git-annex toplevel code
 -}

module Annex (
	startAnnex,
	annexFile,
	unannexFile,
	annexGetFile,
	annexWantFile,
	annexDropFile,
	annexPushRepo,
	annexPullRepo
) where

import System.Posix.Files
import System.Directory
import Data.String.Utils
import List
import GitRepo
import Utility
import Locations
import Backend
import BackendList
import UUID
import LocationLog
import Types

{- On startup, examine the git repo, prepare it, and record state for
 - later. -}
startAnnex :: IO State
startAnnex = do
	r <- gitRepoFromCwd
	r' <- gitConfigRead r
	r'' <- prepUUID r'
	gitSetup r''

	return State {
		repo = r',
		backends = parseBackendList $ gitConfig r' "annex.backends" ""
	}

inBackend file yes no = do
	r <- lookupFile file
	case (r) of
		Just v -> yes v
		Nothing -> no
notinBackend file yes no = inBackend file no yes

{- Annexes a file, storing it in a backend, and then moving it into
 - the annex directory and setting up the symlink pointing to its content. -}
annexFile :: State -> FilePath -> IO ()
annexFile state file = inBackend file err $ do
	checkLegal file
	stored <- storeFile state file
	case (stored) of
		Nothing -> error $ "no backend could store: " ++ file
		Just (key, backend) -> setup key backend
	where
		err = error $ "already annexed " ++ file
		checkLegal file = do
			s <- getSymbolicLinkStatus file
			if ((isSymbolicLink s) || (not $ isRegularFile s))
				then error $ "not a regular file: " ++ file
				else return ()
		setup key backend = do
			logStatus state key ValuePresent
			let dest = annexLocation (repo state) backend key
			let reldest = annexLocationRelative (repo state) backend key
			createDirectoryIfMissing True (parentDir dest)
			renameFile file dest
			createSymbolicLink ((linkTarget file) ++ reldest) file
			gitRun (repo state) ["add", file]
			gitRun (repo state) ["commit", "-m", 
				("git-annex annexed " ++ file), file]
		linkTarget file =
			-- relies on file being relative to the top of the 
			-- git repo; just replace each subdirectory with ".."
			if (subdirs > 0)
				then (join "/" $ take subdirs $ repeat "..") ++ "/"
				else ""
			where
				subdirs = (length $ split "/" file) - 1
		

{- Inverse of annexFile. -}
unannexFile :: State -> FilePath -> IO ()
unannexFile state file = notinBackend file err $ \(key, backend) -> do
	dropFile state backend key
	logStatus state key ValueMissing
	removeFile file
	gitRun (repo state) ["rm", file]
	gitRun (repo state) ["commit", "-m",
		("git-annex unannexed " ++ file), file]
	-- git rm deletes empty directories;
	-- put them back
	createDirectoryIfMissing True (parentDir file)
	let src = annexLocation (repo state) backend key
	renameFile src file
	return ()
	where
		err = error $ "not annexed " ++ file

{- Gets an annexed file from one of the backends. -}
annexGetFile :: State -> FilePath -> IO ()
annexGetFile state file = notinBackend file err $ \(key, backend) -> do
	inannex <- inAnnex state backend key
	if (inannex)
		then return ()
		else do
			let dest = annexLocation (repo state) backend key
			createDirectoryIfMissing True (parentDir dest)
			success <- retrieveFile state backend key dest
			if (success)
				then do
					logStatus state key ValuePresent
					return ()
				else error $ "failed to get " ++ file
	where
		err = error $ "not annexed " ++ file

{- Indicates a file is wanted. -}
annexWantFile :: State -> FilePath -> IO ()
annexWantFile state file = do error "not implemented" -- TODO

{- Indicates a file is not wanted. -}
annexDropFile :: State -> FilePath -> IO ()
annexDropFile state file = do error "not implemented" -- TODO

{- Pushes all files to a remote repository. -}
annexPushRepo :: State -> String -> IO ()
annexPushRepo state reponame = do error "not implemented" -- TODO

{- Pulls all files from a remote repository. -}
annexPullRepo :: State -> String -> IO ()
annexPullRepo state reponame = do error "not implemented" -- TODO

{- Sets up a git repo for git-annex. May be called repeatedly. -}
gitSetup :: GitRepo -> IO ()
gitSetup repo = do
	-- configure git to use union merge driver on state files
	exists <- doesFileExist attributes
	if (not exists)
		then do
			writeFile attributes $ attrLine ++ "\n"
			commit
		else do
			content <- readFile attributes
			if (all (/= attrLine) (lines content))
				then do
					appendFile attributes $ attrLine ++ "\n"
					commit
				else return ()
	where
		attrLine = stateLoc ++ "/*.log merge=union"
		attributes = gitAttributes repo
		commit = do
			gitRun repo ["add", attributes]
			gitRun repo ["commit", "-m", "git-annex setup", 
					attributes]

{- Updates the LocationLog when a key's presence changes. -}
logStatus state key status = do
	f <- logChange (repo state) key (getUUID state (repo state)) status
	gitRun (repo state) ["add", f]
	gitRun (repo state) ["commit", "-m", "git-annex log update", f]

{- Checks if a given key is currently present in the annexLocation -}
inAnnex :: State -> Backend -> Key -> IO Bool
inAnnex state backend key = doesFileExist $ annexLocation (repo state) backend key
