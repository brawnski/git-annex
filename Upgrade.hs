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
	showSideAction "Upgrading object directory layout v1 to v2..."
	error "upgradeFrom1 TODO FIXME"

	-- v2 adds hashing of filenames of content and location log files.
	-- 
	-- Key information is encoded in filenames differently.
	-- 
	-- When upgrading a v1 key to v2, file size metadata needs to be
	-- added to the key (unless it is a WORM key, which encoded
	-- mtime:size in v1). This can only be done when the file content
	-- is present. 
	--
	-- So there are two approaches -- either upgrade
	-- everything, leaving out file size information for files not
	-- present in the current repo; or upgrade peicemeil, only
	-- upgrading keys whose content is present.
	--
	-- The latter approach would mean that, until every clone of an
	-- annex is upgraded, git annex would refuse to operate on annexed
	-- files that had not yet been committed. Unless it were taught to
	-- work with both v1 and v2 keys in the same repo.
	--
	-- Another problem with the latter approach might involve content
	-- being moved between repos while the conversion is still
	-- incomplete. If repo A has already upgraded, and B has not, and B
	-- has K, moving K from B -> A would result in it lurking
	-- unconverted on A. Unless A upgraded it in passing. But that's
	-- getting really complex, and would mean a constant trickle of
	-- upgrade commits, which users would find annoying.
	--
	-- So, the former option it is! Note that file size metadata
	-- will only be used for detecting situations where git-annex
	-- would run out of disk space, so if some keys don't have it,
	-- the impact is small. At least initially. It could be used in the
	-- future by smart auto-repo balancing code, etc.
	--
	-- Anyway, since v2 plans ahead for other metadata being included
	-- in keys, there should probably be a way to update a key.
	-- Something similar to the migrate subcommand could be used,
	-- and users could then run that at their leisure. Or, this upgrade
	-- could to that key update for all keys that have been converted
	-- and have content in the repo.

upgradeFrom0 :: Annex Bool
upgradeFrom0 = do
	showSideAction "Upgrading object directory layout v0 to v1..."
	g <- Annex.gitRepo

	-- do the reorganisation of the files
	let olddir = gitAnnexDir g
	keys <- getKeysPresent0' olddir
	forM_ keys $ \k -> moveAnnex k $ olddir </> keyFile k

	-- update the symlinks to the files
	files <- liftIO $ Git.inRepo g [Git.workTree g]
	fixlinks files
	Annex.queueRun

	-- Few people had v0 repos, so go the long way around from 0 -> 1 -> 2
	upgradeFrom1

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
