{- git-annex v0 -> v1 upgrade support
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Upgrade.V0 where

import System.IO.Error (try)
import System.Directory
import Control.Monad.State (liftIO)
import Control.Monad (filterM, forM_)
import System.Posix.Files
import System.FilePath

import Content
import Types
import Locations
import qualified GitRepo as Git
import qualified Annex
import Messages
import Utility
import qualified Upgrade.V1

upgrade :: Annex Bool
upgrade = do
	showSideAction "Upgrading object directory layout v0 to v1..."
	g <- Annex.gitRepo

	-- do the reorganisation of the key files
	let olddir = gitAnnexDir g
	keys <- getKeysPresent0 olddir
	forM_ keys $ \k -> moveAnnex k $ olddir </> keyFile0 k

	-- update the symlinks to the key files
	files <- liftIO $ Git.inRepo g [Git.workTree g]
	fixlinks files
	Annex.queueRun

	-- Few people had v0 repos, so go the long way around from 0 -> 1 -> 2
	Upgrade.V1.upgrade

	where
		fixlinks [] = return ()
		fixlinks (f:fs) = do
			r <- lookupFile0 f
			case r of
				Nothing -> return ()
				Just (k, _) -> do
					link <- calcGitLink f k
					liftIO $ removeFile f
					liftIO $ createSymbolicLink link f
					Annex.queue "add" [Param "--"] f
			fixlinks fs

-- these stayed unchanged between v0 and v1
keyFile0 :: Key -> FilePath
keyFile0 = Upgrade.V1.keyFile1
fileKey0 :: FilePath -> Key
fileKey0 = Upgrade.V1.fileKey1
lookupFile0 :: FilePath -> Annex (Maybe (Key, Backend Annex))
lookupFile0 = Upgrade.V1.lookupFile1

getKeysPresent0 :: FilePath -> Annex [Key]
getKeysPresent0 dir = do
	exists <- liftIO $ doesDirectoryExist dir
	if (not exists)
		then return []
		else do
			contents <- liftIO $ getDirectoryContents dir
			files <- liftIO $ filterM present contents
			return $ map fileKey0 files
	where
		present d = do
			result <- try $
				getFileStatus $ dir ++ "/" ++ takeFileName d
			case result of
				Right s -> return $ isRegularFile s
				Left _ -> return False
