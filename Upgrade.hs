{- git-annex upgrade support
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Upgrade where

import System.IO.Error (try)
import System.Directory
import Control.Monad.State (liftIO)
import Control.Monad (filterM, forM_)
import System.Posix.Files
import System.FilePath
import Data.Maybe

import Content
import Types
import Locations
import qualified GitRepo as Git
import qualified Annex
import qualified Backend
import Messages
import Version
import Utility

{- Uses the annex.version git config setting to automate upgrades. -}
upgrade :: Annex Bool
upgrade = do
	version <- getVersion
	case version of
		Just "0" -> upgradeFrom0
		Just "1" -> upgradeFrom1
		Nothing -> return True -- repo not initted yet, no version
		Just v | v == currentVersion -> return True
		Just _ -> error "this version of git-annex is too old for this git repository!"

upgradeFrom1 :: Annex Bool
upgradeFrom1 = do
	showSideAction "Upgrading object directory layout..."
	error "upgradeFrom1 TODO FIXME"

upgradeFrom0 :: Annex Bool
upgradeFrom0 = do
	showSideAction "Upgrading object directory layout..."
	g <- Annex.gitRepo

	-- do the reorganisation of the files
	let olddir = gitAnnexDir g
	keys <- getKeysPresent0' olddir
	forM_ keys $ \k -> moveAnnex k $ olddir </> keyFile k

	-- update the symlinks to the files
	files <- liftIO $ Git.inRepo g [Git.workTree g]
	fixlinks files
	Annex.queueRun

	setVersion

	return True

	where
		fixlinks [] = return ()
		fixlinks (f:fs) = do
			r <- Backend.lookupFile f
			case r of
				Nothing -> return ()
				Just (k, _) -> do
					link <- calcGitLink f k
					liftIO $ removeFile f
					liftIO $ createSymbolicLink link f
					Annex.queue "add" [Param "--"] f
			fixlinks fs

getKeysPresent0' :: FilePath -> Annex [Key]
getKeysPresent0' dir = do
	exists <- liftIO $ doesDirectoryExist dir
	if (not exists)
		then return []
		else do
			contents <- liftIO $ getDirectoryContents dir
			files <- liftIO $ filterM present contents
			return $ catMaybes $ map fileKey files
	where
		present d = do
			result <- try $
				getFileStatus $ dir ++ "/" ++ takeFileName d
			case result of
				Right s -> return $ isRegularFile s
				Left _ -> return False
