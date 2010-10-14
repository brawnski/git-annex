{- git-annex subcommands -}

module Commands (
	start,
	annexCmd,
	unannexCmd,
	getCmd,
	wantCmd,
	dropCmd,
	pushCmd,
	pullCmd
) where

import Control.Monad.State (liftIO)
import System.Posix.Files
import System.Directory
import Data.String.Utils
import List
import qualified GitRepo as Git
import qualified Annex
import Utility
import Locations
import qualified Backend
import BackendList
import UUID
import LocationLog
import Types

{- Create and returns an Annex state object. 
 - Examines and prepares the git repo.
 -}
start :: IO AnnexState
start = do
	g <- Git.repoFromCwd
	let s = Annex.new g
	(_,s') <- Annex.run s (prep g)
	return s'
	where
		prep g = do
			-- setup git and read its config; update state
			g' <- liftIO $ Git.configRead g
			Annex.gitRepoChange g'
			liftIO $ gitSetup g'
			Annex.backendsChange $ parseBackendList $
				Git.configGet g' "annex.backends" ""
			prepUUID

inBackend file yes no = do
	r <- liftIO $ Backend.lookupFile file
	case (r) of
		Just v -> yes v
		Nothing -> no
notinBackend file yes no = inBackend file no yes

{- Annexes a file, storing it in a backend, and then moving it into
 - the annex directory and setting up the symlink pointing to its content. -}
annexCmd :: FilePath -> Annex ()
annexCmd file = inBackend file err $ do
	liftIO $ checkLegal file
	stored <- Backend.storeFile file
	g <- Annex.gitRepo
	case (stored) of
		Nothing -> error $ "no backend could store: " ++ file
		Just (key, backend) -> do
			logStatus key ValuePresent
			liftIO $ setup g key backend
	where
		err = error $ "already annexed " ++ file
		checkLegal file = do
			s <- getSymbolicLinkStatus file
			if ((isSymbolicLink s) || (not $ isRegularFile s))
				then error $ "not a regular file: " ++ file
				else return ()
		setup g key backend = do
			let dest = annexLocation g backend key
			let reldest = annexLocationRelative g backend key
			createDirectoryIfMissing True (parentDir dest)
			renameFile file dest
			createSymbolicLink ((linkTarget file) ++ reldest) file
			Git.run g ["add", file]
			Git.run g ["commit", "-m", 
				("git-annex annexed " ++ file), file]
		linkTarget file =
			-- relies on file being relative to the top of the 
			-- git repo; just replace each subdirectory with ".."
			if (subdirs > 0)
				then (join "/" $ take subdirs $ repeat "..") ++ "/"
				else ""
			where
				subdirs = (length $ split "/" file) - 1
		

{- Inverse of annexCmd. -}
unannexCmd :: FilePath -> Annex ()
unannexCmd file = notinBackend file err $ \(key, backend) -> do
	Backend.dropFile backend key
	logStatus key ValueMissing
	g <- Annex.gitRepo
	let src = annexLocation g backend key
	liftIO $ moveout g src
	where
		err = error $ "not annexed " ++ file
		moveout g src = do
			removeFile file
			Git.run g ["rm", file]
			Git.run g ["commit", "-m",
				("git-annex unannexed " ++ file), file]
			-- git rm deletes empty directories;
			-- put them back
			createDirectoryIfMissing True (parentDir file)
			renameFile src file
			return ()

{- Gets an annexed file from one of the backends. -}
getCmd :: FilePath -> Annex ()
getCmd file = notinBackend file err $ \(key, backend) -> do
	inannex <- inAnnex backend key
	if (inannex)
		then return ()
		else do
			g <- Annex.gitRepo
			let dest = annexLocation g backend key
			liftIO $ createDirectoryIfMissing True (parentDir dest)
			success <- Backend.retrieveFile backend key dest
			if (success)
				then do
					logStatus key ValuePresent
					return ()
				else error $ "failed to get " ++ file
	where
		err = error $ "not annexed " ++ file

{- Indicates a file is wanted. -}
wantCmd :: FilePath -> Annex ()
wantCmd file = do error "not implemented" -- TODO

{- Indicates a file is not wanted. -}
dropCmd :: FilePath -> Annex ()
dropCmd file = do error "not implemented" -- TODO

{- Pushes all files to a remote repository. -}
pushCmd :: String -> Annex ()
pushCmd reponame = do error "not implemented" -- TODO

{- Pulls all files from a remote repository. -}
pullCmd :: String -> Annex ()
pullCmd reponame = do error "not implemented" -- TODO

{- Sets up a git repo for git-annex. May be called repeatedly. -}
gitSetup :: Git.Repo -> IO ()
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
		attributes = Git.attributes repo
		commit = do
			Git.run repo ["add", attributes]
			Git.run repo ["commit", "-m", "git-annex setup", 
					attributes]

{- Updates the LocationLog when a key's presence changes. -}
logStatus :: Key -> LogStatus -> Annex ()
logStatus key status = do
	g <- Annex.gitRepo
	u <- getUUID g
	f <- liftIO $ logChange g key u status
	liftIO $ commit g f
	where
		commit g f = do
			Git.run g ["add", f]
			Git.run g ["commit", "-m", "git-annex log update", f]

{- Checks if a given key is currently present in the annexLocation -}
inAnnex :: Backend -> Key -> Annex Bool
inAnnex backend key = do
	g <- Annex.gitRepo
	liftIO $ doesFileExist $ annexLocation g backend key
