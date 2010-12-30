{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Unused where

import Control.Monad.State (liftIO)
import qualified Data.Map as M

import Command
import Types
import Core
import Messages
import Locations
import qualified Annex

seek :: [CommandSeek]
seek = [withNothing start]

{- Finds unused content in the annex. -} 
start :: CommandStartNothing
start = do
	showStart "unused" ""
	return $ Just perform

perform :: CommandPerform
perform = do
	_ <- checkUnused
	return $ Just $ return True

checkUnused :: Annex Bool
checkUnused = do
	showNote "checking for unused data..."
	unused <- unusedKeys
	if null unused
		then return True
		else do
			let list = number 1 unused
			g <- Annex.gitRepo
			liftIO $ writeFile (annexUnusedLog g) $ unlines $ 
				map (\(n, k) -> show n ++ " " ++ show k) list
			showLongNote $ w list
			return False
	where
		w u = unlines $
			["Some annexed data is no longer pointed to by any files in the repository:",
			 "  NUMBER  KEY"]
			++ map cols u ++
			["(To see where data was previously used, try: git log --stat -S'KEY')",
			 "(To remove unwanted data: git-annex dropunused NUMBER)",
			 ""]
		cols (n,k) = "  " ++ pad 6 (show n) ++ "  " ++ show k
		pad n s = s ++ replicate (n - length s) ' '

number :: Integer -> [a] -> [(Integer, a)]
number _ [] = []
number n (x:xs) = (n, x):(number (n+1) xs)

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
		remove a b = foldl (flip M.delete) b a

existsMap :: Ord k => [k] -> M.Map k Int
existsMap l = M.fromList $ map (\k -> (k, 1)) l
