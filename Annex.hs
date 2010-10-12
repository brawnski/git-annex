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

{- An annexed file's content is stored somewhere under .git/annex/,
 - based on the key. Since the symlink is user-visible, the filename
 - used should be as close to the key as possible, in case the key is a
 - filename or url. Just escape "/" in the key name, to keep a flat
 - tree of files and avoid issues with files ending with "/" etc. -}
annexLocation :: State -> Key -> FilePath
annexLocation state key = gitDir (repo state) ++ "/annex/" ++ (transform key)
	where transform s = replace "/" "%" $ replace "%" "%%" s

{- Checks if a given key is currently present in the annexLocation -}
inAnnex :: State -> Key -> IO Bool
inAnnex state key = doesFileExist $ annexLocation state key

{- On startup, examine the git repo, prepare it, and record state for
 - later. -}
startAnnex :: IO State
startAnnex = do
	r <- gitRepoFromCwd
	r' <- prepUUID r
	gitPrep r'

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
				Just key -> symlink key
	where
		symlink key = do
			let dest = annexLocation state key
			createDirectoryIfMissing True (parentDir dest)
			renameFile file dest
			logChange (repo state) file (getUUID (repo state)) FilePresent
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
	alreadyannexed <- lookupBackend state file
	case (alreadyannexed) of
		Nothing -> error $ "not annexed " ++ file
		Just _ -> do
			mkey <- dropFile state file
			case (mkey) of
				Nothing -> return ()
				Just key -> do
					let src = annexLocation state key
					removeFile file
					renameFile src file
					return ()

{- Transfers the file from a remote. -}
annexGetFile :: State -> FilePath -> IO ()
annexGetFile state file = do
	alreadyannexed <- lookupBackend state file
	case (alreadyannexed) of
		Nothing -> error $ "not annexed " ++ file
		Just backend -> do
			key <- lookupKey state backend file
			inannex <- inAnnex state key
			if (inannex)
				then return ()
				else do
					let dest = annexLocation state key
					createDirectoryIfMissing True (parentDir dest)
					success <- retrieveFile state file dest
					if (success)
						then return ()
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
gitPrep :: GitRepo -> IO ()
gitPrep repo = do
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
