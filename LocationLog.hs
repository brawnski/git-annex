{- git-annex location log
 -
 - git-annex keeps track of which repositories have the contents of annexed
 - files.
 -
 - Location tracking information is stored in `.git-annex/key.log`.
 - Repositories record their UUID and the date when they --get or --drop
 - a value.
 -
 - A line of the log will look like: "date N UUID"
 - Where N=1 when the repo has the file, and 0 otherwise.
 - 
 - Git is configured to use a union merge for this file,
 - so the lines may be in arbitrary order, but it will never conflict.
 -
 - Copyright 2010-2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module LocationLog (
	LogStatus(..),
	logChange,
	readLog,
	writeLog,
	keyLocations,
	loggedKeys,
	logFile
) where

import Data.Time.Clock.POSIX
import Data.Time
import System.Locale
import System.Directory
import System.FilePath
import qualified Data.Map as Map
import Control.Monad (when)
import Data.Maybe

import qualified GitRepo as Git
import Utility
import UUID
import Types
import Locations

data LogLine = LogLine {
	date :: POSIXTime,
	status :: LogStatus,
	uuid :: UUID
} deriving (Eq)

data LogStatus = ValuePresent | ValueMissing | Undefined
	deriving (Eq)

instance Show LogStatus where
	show ValuePresent = "1"
	show ValueMissing = "0"
	show Undefined = "undefined"

instance Read LogStatus where
	readsPrec _ "1" = [(ValuePresent, "")]
	readsPrec _ "0" = [(ValueMissing, "")]
	readsPrec _ _   = [(Undefined, "")]

instance Show LogLine where
	show (LogLine d s u) = unwords [show d, show s, u]

instance Read LogLine where
	-- This parser is robust in that even unparsable log lines are
	-- read without an exception being thrown.
	-- Such lines have a status of Undefined.
	readsPrec _ string = 
		if length w == 3
			then case pdate of
				Just v -> good v
				Nothing -> bad
			else bad
		where
			w = words string
			s = read $ w !! 1
			u = w !! 2
			pdate :: Maybe UTCTime
			pdate = parseTime defaultTimeLocale "%s%Qs" $ head w

			good v = ret $ LogLine (utcTimeToPOSIXSeconds v) s u
			bad = ret $ LogLine 0 Undefined ""
			ret v = [(v, "")]

{- Log a change in the presence of a key's value in a repository,
 - and returns the filename of the logfile. -}
logChange :: Git.Repo -> Key -> UUID -> LogStatus -> IO FilePath
logChange repo key u s = do
	when (null u) $
		error $ "unknown UUID for " ++ Git.repoDescribe repo ++ 
			" (have you run git annex init there?)"
	line <- logNow s u
	let f = logFile repo key
	ls' <- readLog $ logFileOld repo key
	ls <- readLog f
	writeLog f (compactLog $ line:ls'++ls)
	return f

{- Reads a log file.
 - Note that the LogLines returned may be in any order. -}
readLog :: FilePath -> IO [LogLine]
readLog file = catch (return . parseLog =<< readFile file) (const $ return [])

parseLog :: String -> [LogLine]
parseLog s = filter parsable $ map read $ lines s
	where
		-- some lines may be unparseable, avoid them
		parsable l = status l /= Undefined

{- Writes a set of lines to a log file -}
writeLog :: FilePath -> [LogLine] -> IO ()
writeLog file ls = safeWriteFile file (unlines $ map show ls)

{- Generates a new LogLine with the current date. -}
logNow :: LogStatus -> UUID -> IO LogLine
logNow s u = do
	now <- getPOSIXTime
	return $ LogLine now s u

{- Returns a list of repository UUIDs that, according to the log, have
 - the value of a key. -}
keyLocations :: Git.Repo -> Key -> IO [UUID]
keyLocations thisrepo key = do
	ls <- readLog $ logFile thisrepo key
	ls' <- readLog $ logFileOld thisrepo key
	return $ map uuid $ filterPresent $ ls'++ls

{- Filters the list of LogLines to find ones where the value
 - is (or should still be) present. -}
filterPresent :: [LogLine] -> [LogLine]
filterPresent ls = filter (\l -> ValuePresent == status l) $ compactLog ls

type LogMap = Map.Map UUID LogLine

{- Compacts a set of logs, returning a subset that contains the current
 - status. -}
compactLog :: [LogLine] -> [LogLine]
compactLog ls = compactLog' Map.empty ls
compactLog' :: LogMap -> [LogLine] -> [LogLine]
compactLog' m [] = Map.elems m
compactLog' m (l:ls) = compactLog' (mapLog m l) ls

{- Inserts a log into a map of logs, if the log has better (ie, newer)
 - information about a repo than the other logs in the map -}
mapLog :: LogMap -> LogLine -> LogMap
mapLog m l = 
	if better
		then Map.insert u l m
		else m
	where
		better = case Map.lookup u m of
			Just l' -> (date l' <= date l)
			Nothing -> True
		u = uuid l

{- Finds all keys that have location log information. -}
loggedKeys :: Git.Repo -> IO [Key]
loggedKeys repo = do
	let dir = gitStateDir repo
	exists <- doesDirectoryExist dir
	if exists
		then do
			-- 2 levels of hashing
			levela <- dirContents dir
			levelb <- mapM tryDirContents levela
			files <- mapM tryDirContents (concat levelb)
			return $ catMaybes $
				map (logFileKey . takeFileName) (concat files)
		else return []
	where
		tryDirContents d = catch (dirContents d) (return . const [])
