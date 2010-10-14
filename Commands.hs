{- git-annex command line -}

module Commands (argvToActions) where

import System.Console.GetOpt
import Control.Monad.State (liftIO)
import System.Posix.Files
import System.Directory
import Data.String.Utils
import List
import IO
import qualified GitRepo as Git
import qualified Annex
import Utility
import Locations
import qualified Backend
import BackendList
import UUID
import LocationLog
import Types
import Core
import qualified Remotes

options :: [OptDescr (String -> Annex ())]
options =
	[ Option ['a'] ["add"]	(NoArg addCmd)	"add files to annex"
	, Option ['p'] ["push"]	(NoArg pushCmd)	"push annex to repos"
	, Option ['P'] ["pull"]	(NoArg pullCmd)	"pull annex from repos"
	, Option ['w'] ["want"]	(NoArg wantCmd)	"request file contents"
	, Option ['g'] ["get"]	(NoArg getCmd)	"transfer file contents"
	, Option ['d'] ["drop"]	(NoArg dropCmd)	"indicate file contents not needed"
	, Option ['u'] ["unannex"] (NoArg unannexCmd) "undo --add"
	]

{- Parses command line and returns a list of actons to be run in the Annex
 - monad. -}
argvToActions :: [String] -> IO [Annex ()]
argvToActions argv = do
	case getOpt Permute options argv of
		([],files,[]) -> return $ map defaultCmd files
		-- one mode is normal case
		(m:[],files,[]) -> return $ map m files
		-- multiple modes is an error
		(ms,files,[]) -> ioError (userError ("only one mode should be specified\n" ++ usageInfo header options))
		-- error case
		(_,files,errs) -> ioError (userError (concat errs ++ usageInfo header options))
	where header = "Usage: git-annex [mode] file"

{- Default mode is to annex a file if it is not already, and otherwise
 - get its content. -}
defaultCmd :: FilePath -> Annex ()
defaultCmd file = do
	r <- liftIO $ Backend.lookupFile file
	case (r) of
		Just v -> getCmd file
		Nothing -> addCmd file

{- Annexes a file, storing it in a backend, and then moving it into
 - the annex directory and setting up the symlink pointing to its content. -}
addCmd :: FilePath -> Annex ()
addCmd file = inBackend file err $ do
	liftIO $ checkLegal file
	stored <- Backend.storeFileKey file
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
		

{- Inverse of addCmd. -}
unannexCmd :: FilePath -> Annex ()
unannexCmd file = notinBackend file err $ \(key, backend) -> do
	Backend.removeKey backend key
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
			success <- Backend.retrieveKeyFile backend key dest
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
dropCmd file = notinBackend file err $ \(key, backend) -> do
	requireEnoughCopies key
	success <- Backend.removeKey backend key
	if (success)
		then do
			logStatus key ValueMissing
			inannex <- inAnnex backend key
			if (inannex)
				then do
					g <- Annex.gitRepo
					let loc = annexLocation g backend key
					liftIO $ removeFile loc
					return ()
				else return ()
		else error $ "backend refused to drop " ++ file
	where
		err = error $ "not annexed " ++ file

{- Pushes all files to a remote repository. -}
pushCmd :: String -> Annex ()
pushCmd reponame = do error "not implemented" -- TODO

{- Pulls all files from a remote repository. -}
pullCmd :: String -> Annex ()
pullCmd reponame = do error "not implemented" -- TODO

{- Updates the LocationLog when a key's presence changes. -}
logStatus :: Key -> LogStatus -> Annex ()
logStatus key status = do
	g <- Annex.gitRepo
	u <- getUUID g
	f <- liftIO $ logChange g key u status
	liftIO $ Git.run g ["add", f] -- committed at shutdown

inBackend file yes no = do
	r <- liftIO $ Backend.lookupFile file
	case (r) of
		Just v -> yes v
		Nothing -> no
notinBackend file yes no = inBackend file no yes

{- Checks remotes to verify that enough copies of a key exist to allow
 - for a key to be safely removed (with no data loss), and fails with an
 - error if not. -}
requireEnoughCopies :: Key -> Annex ()
requireEnoughCopies key = do
	g <- Annex.gitRepo
	let numcopies = read $ Git.configGet g config "1"
	remotes <- Remotes.withKey key
	if (numcopies > length remotes)
		then error $ "I only know about " ++ (show $ length remotes) ++ 
			" out of " ++ (show numcopies) ++ 
			" necessary copies of: " ++ (keyFile key) ++
			unsafe
		else findcopies numcopies remotes []
	where
		findcopies 0 _ _ = return () -- success, enough copies found
		findcopies _ [] bad = die bad
		findcopies n (r:rs) bad = do
			result <- liftIO $ try $ haskey r
			case (result) of
				Right True	-> findcopies (n-1) rs bad
				Right False	-> findcopies n rs bad
				Left _		-> findcopies n rs (r:bad)
		haskey r = do
			-- To check if a remote has a key, construct a new
			-- Annex monad and query its backend.
			a <- Annex.new r
			(result, _) <- Annex.run a (Backend.hasKey key)
			return result
		die bad =
			error $ "I failed to find enough other copies of: " ++
				(keyFile key) ++
				(if (0 /= length bad) then listbad bad else "")
				++ unsafe
		listbad bad = "\nI was unable to access these remotes: " ++
				(Remotes.list bad) 
		unsafe = "\n  -- According to the " ++ config ++
			" setting, it is not safe to remove it!"
		config = "annex.numcopies"
