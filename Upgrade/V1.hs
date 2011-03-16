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
import Key
import System.Posix.Types

import Content
import Types
import Locations
import qualified Annex
import Backend
import Messages
import Version

upgrade :: Annex Bool
upgrade = do
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
	
	-- do the reorganisation of the log files
	
	-- do the reorganisation of the key files
	g <- Annex.gitRepo
	let olddir = gitAnnexDir g
	keys <- getKeysPresent1
	forM_ keys $ \k -> moveAnnex k $ olddir </> keyFile1 k
	
	-- update the symlinks to the key files

	Annex.queueRun
	
	setVersion

	return True

keyFile1 :: Key -> FilePath
keyFile1 key = replace "/" "%" $ replace "%" "&s" $ replace "&" "&a"  $ show key

fileKey1 :: FilePath -> Key
fileKey1 file = readKey1 $
	replace "&a" "&" $ replace "&s" "%" $ replace "%" "/" file

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
