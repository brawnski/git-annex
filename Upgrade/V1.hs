{- git-annex v1 -> v2 upgrade support
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Upgrade.V1 where

import System.IO.Error (try)
import System.Directory
import Control.Monad.State (liftIO)
import Control.Monad (filterM, forM_, unless)
import System.Posix.Files
import System.FilePath
import Data.String.Utils
import System.Posix.Types
import Data.Maybe

import Key
import Content
import Types
import Locations
import LocationLog
import qualified Annex
import qualified GitRepo as Git
import Backend
import Messages
import Version
import Utility
import qualified Command.Init

-- v2 adds hashing of filenames of content and location log files.
-- Key information is encoded in filenames differently, so
-- both content and location log files move around, and symlinks
-- to content need to be changed.
-- 
-- When upgrading a v1 key to v2, file size metadata ought to be
-- added to the key (unless it is a WORM key, which encoded
-- mtime:size in v1). This can only be done when the file content
-- is present. Since upgrades need to happen consistently, 
-- (so that two repos get changed the same way by the upgrade, and
-- will merge), that metadata cannot be added on upgrade.
--
-- Note that file size metadata
-- will only be used for detecting situations where git-annex
-- would run out of disk space, so if some keys don't have it,
-- the impact is minor. At least initially. It could be used in the
-- future by smart auto-repo balancing code, etc.
--
-- Anyway, since v2 plans ahead for other metadata being included
-- in keys, there should probably be a way to update a key.
-- Something similar to the migrate subcommand could be used,
-- and users could then run that at their leisure.

upgrade :: Annex Bool
upgrade = do
	showSideAction "Upgrading object directory layout v1 to v2..."

	g <- Annex.gitRepo
	if Git.repoIsLocalBare g
		then do
			moveContent
		else do
			moveContent
			updateSymlinks
			moveLocationLogs
	
			Annex.queueRun

			-- add new line to auto-merge hashed location logs
			-- this commits, so has to come after the upgrade
			liftIO $ Command.Init.gitAttributesWrite g

	setVersion
	return True

moveContent :: Annex ()
moveContent = do
	keys <- getKeysPresent1
	forM_ keys move
	where
		move k = do
			g <- Annex.gitRepo
			let f = gitAnnexObjectDir g </> keyFile1 k </> keyFile1 k
			let d = parentDir f
			liftIO $ allowWrite d
			liftIO $ allowWrite f
			moveAnnex k f
			liftIO $ removeDirectory d

updateSymlinks :: Annex ()
updateSymlinks = do
	g <- Annex.gitRepo
	files <- liftIO $ Git.inRepo g [Git.workTree g]
	forM_ files $ fixlink
	where
		fixlink f = do
			r <- lookupFile1 f
			case r of
				Nothing -> return ()
				Just (k, _) -> do
					link <- calcGitLink f k
					liftIO $ removeFile f
					liftIO $ createSymbolicLink link f
					Annex.queue "add" [Param "--"] f
					Annex.queueRunAt 1024

moveLocationLogs :: Annex ()
moveLocationLogs = do
	logkeys <- oldlocationlogs
	forM_ logkeys move
		where
			oldlocationlogs = do
				g <- Annex.gitRepo
				let dir = gitStateDir g
				contents <- liftIO $ getDirectoryContents dir
				return $ catMaybes $ map oldlog2key contents
			move (l, k) = do
				g <- Annex.gitRepo
				let dest = logFile g k
				let dir = gitStateDir g
				let f = dir </> l
				liftIO $ createDirectoryIfMissing True (parentDir dest)
				-- could just git mv, but this way deals with
				-- log files that are not checked into git,
				-- as well as merging with already upgraded
				-- logs that have been pulled from elsewhere
				old <- liftIO $ readLog f
				new <- liftIO $ readLog dest
				liftIO $ writeLog dest (old++new)
				Annex.queue "add" [Param "--"] dest
				Annex.queue "add" [Param "--"] f
				Annex.queue "rm" [Param "--quiet", Param "-f", Param "--"] f
				Annex.queueRunAt 1024
		
oldlog2key :: FilePath -> Maybe (FilePath, Key)
oldlog2key l = 
	let len = length l - 4 in
		if drop len l == ".log"
		then let k = readKey1 (take len l) in
			if null (keyName k) || null (keyBackendName k)
			then Nothing
			else Just (l, k)
		else Nothing

-- WORM backend keys: "WORM:mtime:size:filename"
-- all the rest: "backend:key"
readKey1 :: String -> Key
readKey1 v = Key { keyName = n , keyBackendName = b, keySize = s, keyMtime = t }
	where
		bits = split ":" v
		b = head bits
		n = join ":" $ drop (if wormy then 3 else 1) bits
		t = if wormy
			then Just (read (bits !! 1) :: EpochTime)
			else Nothing
		s = if wormy
			then Just (read (bits !! 2) :: Integer)
			else Nothing
		wormy = b == "WORM"

showKey1 :: Key -> String
showKey1 Key { keyName = n , keyBackendName = b, keySize = s, keyMtime = t } =
	join ":" $ filter (not . null) [b, showifhere t, showifhere s, n]
		where
			showifhere Nothing = ""
			showifhere (Just v) = show v

keyFile1 :: Key -> FilePath
keyFile1 key = replace "/" "%" $ replace "%" "&s" $ replace "&" "&a"  $ showKey1 key

fileKey1 :: FilePath -> Key
fileKey1 file = readKey1 $
	replace "&a" "&" $ replace "&s" "%" $ replace "%" "/" file

logFile1 :: Git.Repo -> Key -> String
logFile1 repo key = gitStateDir repo ++ keyFile1 key ++ ".log"

lookupFile1 :: FilePath -> Annex (Maybe (Key, Backend Annex))
lookupFile1 file = do
	bs <- Annex.getState Annex.supportedBackends
	tl <- liftIO $ try getsymlink
	case tl of
		Left _ -> return Nothing
		Right l -> makekey bs l
	where
		getsymlink = do
			l <- readSymbolicLink file
			return $ takeFileName l
		makekey bs l = do
			case maybeLookupBackendName bs bname of
				Nothing -> do
					unless (null kname || null bname ||
					        not (isLinkToAnnex l)) $
						warning skip
					return Nothing
				Just backend -> return $ Just (k, backend)
			where
				k = fileKey1 l
				bname = keyBackendName k
				kname = keyName k
				skip = "skipping " ++ file ++ 
					" (unknown backend " ++ bname ++ ")"

getKeysPresent1 :: Annex [Key]
getKeysPresent1  = do
	g <- Annex.gitRepo
	getKeysPresent1' $ gitAnnexObjectDir g
getKeysPresent1' :: FilePath -> Annex [Key]
getKeysPresent1' dir = do
	exists <- liftIO $ doesDirectoryExist dir
	if (not exists)
		then return []
		else do
			contents <- liftIO $ getDirectoryContents dir
			files <- liftIO $ filterM present contents
			return $ map fileKey1 files
	where
		present d = do
			result <- try $
				getFileStatus $ dir ++ "/" ++ d ++ "/"  ++ takeFileName d
			case result of
				Right s -> return $ isRegularFile s
				Left _ -> return False
