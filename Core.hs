{- git-annex core functions
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Core where

import System.IO.Error (try)
import System.Directory
import Control.Monad.State (liftIO)
import System.Path
import Control.Monad (when, unless, filterM)
import System.Posix.Files
import Data.Maybe
import System.FilePath

import Types
import Locations
import LocationLog
import UUID
import qualified GitRepo as Git
import qualified GitQueue
import qualified Annex
import qualified Backend
import Utility
import Messages

{- Runs a list of Annex actions. Catches IO errors and continues
 - (but explicitly thrown errors terminate the whole command).
 - Runs shutdown and propigates an overall error status at the end.
 -}
tryRun :: AnnexState -> [Annex Bool] -> IO ()
tryRun state actions = tryRun' state 0 actions
tryRun' :: AnnexState -> Integer -> [Annex Bool] -> IO ()
tryRun' state errnum (a:as) = do
	result <- try $ Annex.run state a
	case result of
		Left err -> do
			Annex.eval state $ showErr err
			tryRun' state (errnum + 1) as
		Right (True,state') -> tryRun' state' errnum as
		Right (False,state') -> tryRun' state' (errnum + 1) as
tryRun' state errnum [] = do
	_ <- try $ Annex.run state $ shutdown errnum
	when (errnum > 0) $ error $ show errnum ++ " failed"
			
{- Actions to perform each time ran. -}
startup :: Annex Bool
startup = do
	prepUUID
	return True

{- When git-annex is done, it runs this. -}
shutdown :: Integer -> Annex Bool
shutdown errnum = do
	q <- Annex.queueGet
	unless (q == GitQueue.empty) $ do
		showSideAction "Recording state in git..."
		Annex.queueRun

	-- If nothing failed, clean up any files left in the temp directory,
	-- but leave the directory itself. If something failed, temp files
	-- are left behind to allow resuming on re-run.
	when (errnum == 0) $ do
		g <- Annex.gitRepo
		let tmp = annexTmpLocation g
		exists <- liftIO $ doesDirectoryExist tmp
		when exists $ liftIO $ removeDirectoryRecursive tmp
		liftIO $ createDirectoryIfMissing True tmp

	return True

{- Checks if a given key is currently present in the annexLocation. -}
inAnnex :: Key -> Annex Bool
inAnnex key = do
	g <- Annex.gitRepo
	when (Git.repoIsUrl g) $ error "inAnnex cannot check remote repo"
	liftIO $ doesFileExist $ annexLocation g key

{- Calculates the relative path to use to link a file to a key. -}
calcGitLink :: FilePath -> Key -> Annex FilePath
calcGitLink file key = do
	g <- Annex.gitRepo
	cwd <- liftIO $ getCurrentDirectory
	let absfile = case absNormPath cwd file of
		Just f -> f
		Nothing -> error $ "unable to normalize " ++ file
	return $ relPathDirToDir (parentDir absfile) (Git.workTree g) ++
		annexLocationRelative key

{- Updates the LocationLog when a key's presence changes. -}
logStatus :: Key -> LogStatus -> Annex ()
logStatus key status = do
	g <- Annex.gitRepo
	u <- getUUID g
	logfile <- liftIO $ logChange g key u status
	Annex.queue "add" ["--"] logfile

{- Runs an action, passing it a temporary filename to download,
 - and if the action succeeds, moves the temp file into 
 - the annex as a key's content. -}
getViaTmp :: Key -> (FilePath -> Annex Bool) -> Annex Bool
getViaTmp key action = do
	g <- Annex.gitRepo
	let tmp = annexTmpLocation g ++ keyFile key
	liftIO $ createDirectoryIfMissing True (parentDir tmp)
	success <- action tmp
	if success
		then do
			moveAnnex key tmp
			logStatus key ValuePresent
			return True
		else do
			-- the tmp file is left behind, in case caller wants
			-- to resume its transfer
			return False

{- Removes the write bits from a file. -}
preventWrite :: FilePath -> IO ()
preventWrite f = unsetFileMode f writebits
	where
		writebits = foldl unionFileModes ownerWriteMode
					[groupWriteMode, otherWriteMode]

{- Turns a file's write bit back on. -}
allowWrite :: FilePath -> IO ()
allowWrite f = do
	s <- getFileStatus f
	setFileMode f $ fileMode s `unionFileModes` ownerWriteMode

{- Moves a file into .git/annex/objects/ -}
moveAnnex :: Key -> FilePath -> Annex ()
moveAnnex key src = do
	g <- Annex.gitRepo
	let dest = annexLocation g key
	let dir = parentDir dest
	liftIO $ do
		createDirectoryIfMissing True dir
		allowWrite dir -- in case the directory already exists
		renameFile src dest
		preventWrite dest
		preventWrite dir

{- Removes a key's file from .git/annex/objects/ -}
removeAnnex :: Key -> Annex ()
removeAnnex key = do
	g <- Annex.gitRepo
	let file = annexLocation g key
	let dir = parentDir file
	liftIO $ do
		allowWrite dir
		removeFile file
		removeDirectory dir

{- Moves a key's file out of .git/annex/objects/ -}
fromAnnex :: Key -> FilePath -> Annex ()
fromAnnex key dest = do
	g <- Annex.gitRepo
	let file = annexLocation g key
	let dir = parentDir file
	liftIO $ do
		allowWrite dir
		allowWrite file
		renameFile file dest
		removeDirectory dir

{- Moves a key out of .git/annex/objects/ into .git/annex/bad, and
 - returns the file it was moved to. -}
moveBad :: Key -> Annex FilePath
moveBad key = do
	g <- Annex.gitRepo
	let src = annexLocation g key
	let dest = annexBadLocation g ++ takeFileName src
	liftIO $ createDirectoryIfMissing True dest
	liftIO $ renameFile src dest
	liftIO $ removeDirectory (parentDir src)
	return dest

{- List of keys whose content exists in .git/annex/objects/ -}
getKeysPresent :: Annex [Key]
getKeysPresent = do
	g <- Annex.gitRepo
	getKeysPresent' $ annexObjectDir g
getKeysPresent' :: FilePath -> Annex [Key]
getKeysPresent' dir = do
	exists <- liftIO $ doesDirectoryExist dir
	if (not exists)
		then return []
		else do
			contents <- liftIO $ getDirectoryContents dir
			files <- liftIO $ filterM present contents
			return $ map fileKey files
	where
		present d = do
			result <- try $
				getFileStatus $ dir ++ "/" ++ d ++ "/"  ++ takeFileName d
			case result of
				Right s -> return $ isRegularFile s
				Left _ -> return False

{- List of keys referenced by symlinks in the git repo. -}
getKeysReferenced :: Annex [Key]
getKeysReferenced = do
	g <- Annex.gitRepo
	files <- liftIO $ Git.inRepo g [Git.workTree g]
	keypairs <- mapM Backend.lookupFile files
	return $ map fst $ catMaybes keypairs
