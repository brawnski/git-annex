{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Fsck where

import qualified Data.Map as M
import System.Directory
import System.Posix.Files
import Monad (filterM)
import Control.Monad.State (liftIO)
import Data.Maybe

import Command
import Types
import Core
import Locations
import qualified Annex
import qualified GitRepo as Git
import qualified Backend

{- Checks the whole annex for problems. -}
start :: SubCmdStart
start = do
	showStart "fsck" ""
	return $ Just perform

perform :: SubCmdPerform
perform = do
	ok <- checkUnused
	if (ok)
		then return $ Just $ return True
		else do
			showLongNote "Possible problems detected."
			return Nothing

checkUnused :: Annex Bool
checkUnused = do
	showNote "checking for unused data..."
	unused <- unusedKeys
	if (null unused)
		then return True
		else do
			showLongNote $ w unused
			return False
	where
		w u = unlines $ [
			"Some annexed data is no longer pointed to by any files in the repository.",
			"If this data is no longer needed, it can be removed using git-annex dropkey:"
			] ++ map (\k -> "  " ++ show k) u

{- Finds keys whose content is present, but that do not seem to be used
 - by any files in the git repo. -}
unusedKeys :: Annex [Key]
unusedKeys = do
	present <- getKeysPresent
	referenced <- getKeysReferenced
	
	-- Constructing a single map, of the set that tends to be smaller,
	-- appears more efficient in both memory and CPU than constructing
	-- and taking the M.difference of two maps.
	let present_m = existsMap present
	let unused_m = remove referenced present_m
	return $ M.keys unused_m
	where
		remove [] m = m
		remove (x:xs) m = remove xs $ M.delete x m

existsMap :: Ord k => [k] -> M.Map k Int
existsMap l = M.fromList $ map (\k -> (k, 1)) l

getKeysPresent :: Annex [Key]
getKeysPresent = do
	g <- Annex.gitRepo
	let top = annexDir g
	contents <- liftIO $ getDirectoryContents top
	files <- liftIO $ filterM (isreg top) contents
	return $ map fileKey files
	where
		isreg top f = do
			s <- getFileStatus $ top ++ "/" ++ f
			return $ isRegularFile s

getKeysReferenced :: Annex [Key]
getKeysReferenced = do
	g <- Annex.gitRepo
	files <- liftIO $ Git.inRepo g $ Git.workTree g
	keypairs <- mapM Backend.lookupFile files
	return $ map fst $ catMaybes keypairs
