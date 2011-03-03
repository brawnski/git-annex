{- git-annex file content managing
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Content (
	inAnnex,
	calcGitLink,
	logStatus,
	getViaTmp,
	preventWrite,
	allowWrite,
	moveAnnex,
	removeAnnex,
	fromAnnex,
	moveBad,
	getKeysPresent
) where

import System.IO.Error (try)
import System.Directory
import Control.Monad.State (liftIO)
import System.Path
import Control.Monad (when, unless, filterM)
import System.Posix.Files
import System.FilePath

import Types
import Locations
import LocationLog
import UUID
import qualified GitRepo as Git
import qualified Annex
import Utility
import Messages

{- Checks if a given key is currently present in the gitAnnexLocation. -}
inAnnex :: Key -> Annex Bool
inAnnex key = do
	g <- Annex.gitRepo
	when (Git.repoIsUrl g) $ error "inAnnex cannot check remote repo"
	liftIO $ doesFileExist $ gitAnnexLocation g key

{- Calculates the relative path to use to link a file to a key. -}
calcGitLink :: FilePath -> Key -> Annex FilePath
calcGitLink file key = do
	g <- Annex.gitRepo
	cwd <- liftIO $ getCurrentDirectory
	let absfile = case absNormPath cwd file of
		Just f -> f
		Nothing -> error $ "unable to normalize " ++ filePathToString file
	return $ relPathDirToDir (parentDir absfile) 
			(Git.workTree g) </> ".git" </> annexLocation key

{- Updates the LocationLog when a key's presence changes.
 -
 - Note that the LocationLog is not updated in bare repositories.
 - Operations that change a bare repository should be done from
 - a non-bare repository, and the LocationLog in that repository be
 - updated instead. -}
logStatus :: Key -> LogStatus -> Annex ()
logStatus key status = do
	g <- Annex.gitRepo
	unless (Git.repoIsLocalBare g) $ do
		u <- getUUID g
		logfile <- liftIO $ logChange g key u status
		Annex.queue "add" [Param "--"] logfile

{- Runs an action, passing it a temporary filename to download,
 - and if the action succeeds, moves the temp file into 
 - the annex as a key's content. -}
getViaTmp :: Key -> (FilePath -> Annex Bool) -> Annex Bool
getViaTmp key action = do
	g <- Annex.gitRepo
	let tmp = gitAnnexTmpLocation g key
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
	let dest = gitAnnexLocation g key
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
	let file = gitAnnexLocation g key
	let dir = parentDir file
	liftIO $ do
		allowWrite dir
		removeFile file
		removeDirectory dir

{- Moves a key's file out of .git/annex/objects/ -}
fromAnnex :: Key -> FilePath -> Annex ()
fromAnnex key dest = do
	g <- Annex.gitRepo
	let file = gitAnnexLocation g key
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
	let src = gitAnnexLocation g key
	let dest = gitAnnexBadDir g </> takeFileName src
	liftIO $ createDirectoryIfMissing True (parentDir dest)
	liftIO $ allowWrite (parentDir src)
	liftIO $ renameFile src dest
	liftIO $ removeDirectory (parentDir src)
	return dest

{- List of keys whose content exists in .git/annex/objects/ -}
getKeysPresent :: Annex [Key]
getKeysPresent = do
	g <- Annex.gitRepo
	getKeysPresent' $ gitAnnexObjectDir g
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
