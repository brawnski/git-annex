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
import GitRepo
import Utility
import Locations
import Backend
import BackendList
import UUID
import LocationLog
import Types

{- Checks if a given key is currently present in the annexLocation -}
inAnnex :: State -> Backend -> Key -> IO Bool
inAnnex state backend key = doesFileExist $ annexLocation state backend key

{- On startup, examine the git repo, prepare it, and record state for
 - later. -}
startAnnex :: IO State
startAnnex = do
	r <- gitRepoFromCwd
	r' <- prepUUID r
	gitSetup r'

	return State {
		repo = r',
		backends = parseBackendList $ gitConfig r' "annex.backends" ""
	}

{- Annexes a file, storing it in a backend, and then moving it into
 - the annex directory and setting up the symlink pointing to its content. -}
annexFile :: State -> FilePath -> IO ()
annexFile state file = do
	alreadyannexed <- lookupBackend state file
	case (alreadyannexed) of
		Just _ -> error $ "already annexed: " ++ file
		Nothing -> do
			checkLegal file
			stored <- storeFile state file
			case (stored) of
				Nothing -> error $ "no backend could store: " ++ file
				Just (key, backend) -> setup key backend
	where
		checkLegal file = do
			s <- getSymbolicLinkStatus file
			if ((isSymbolicLink s) || (not $ isRegularFile s))
				then error $ "not a regular file: " ++ file
				else return ()
		setup key backend = do
			let dest = annexLocation state backend key
			let reldest = annexLocationRelative state backend key
			createDirectoryIfMissing True (parentDir dest)
			renameFile file dest
			createSymbolicLink ((linkTarget file) ++ reldest) file
			gitRun (repo state) ["add", file]
			gitRun (repo state) ["commit", "-m", 
				("git-annex annexed " ++ file), file]
			logStatus state key ValuePresent
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
unannexFile state file = do
	alreadyannexed <- lookupBackend state file
	case (alreadyannexed) of
		Nothing -> error $ "not annexed " ++ file
		Just _ -> do
			mkey <- dropFile state file
			case (mkey) of
				Nothing -> return ()
				Just (key, backend) -> do
					let src = annexLocation state backend file
					removeFile file
					gitRun (repo state) ["rm", file]
					gitRun (repo state) ["commit", "-m",
						("git-annex unannexed " ++ file),
						file]
					renameFile src file
					logStatus state key ValueMissing
					return ()

{- Transfers the file from a remote. -}
annexGetFile :: State -> FilePath -> IO ()
annexGetFile state file = do
	alreadyannexed <- lookupBackend state file
	case (alreadyannexed) of
		Nothing -> error $ "not annexed " ++ file
		Just backend -> do
			key <- fileKey file
			inannex <- inAnnex state backend key
			if (inannex)
				then return ()
				else do
					let dest = annexLocation state backend key
					createDirectoryIfMissing True (parentDir dest)
					success <- retrieveFile state file dest
					if (success)
						then do
							logStatus state key ValuePresent
							return ()
						else error $ "failed to get " ++ file

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
	f <- logChange (repo state) key (getUUID (repo state)) status
	gitRun (repo state) ["add", f]
	gitRun (repo state) ["commit", "-m", "git-annex log update", f]
