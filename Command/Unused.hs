{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Unused where

import qualified Data.Map as M

import Command
import Types
import Core
import Messages

seek :: [SubCmdSeek]
seek = [withNothing start]

{- Finds unused content in the annex. -} 
start :: SubCmdStartNothing
start = do
	showStart "unused" ""
	return $ Just perform

perform :: SubCmdPerform
perform = do
	ok <- checkUnused
	if ok
		then return $ Just $ return True
		else return Nothing

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
