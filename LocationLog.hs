{- git-annex location log
 -
 - git-annex keeps track of on which repository it last saw a file's content.
 - This can be useful when using it for archiving with offline storage.
 - When you indicate you --want a file, git-annex will tell you which
 - repositories have the file's content.
 -
 - Location tracking information is stored in `.git-annex/filename.log`.
 - Repositories record their UUID and the date when they --get or --drop
 - a file's content.
 -
 - A line of the log will look like: "date N UUID"
 - Where N=1 when the repo has the file, and 0 otherwise.
 - 
 - Git is configured to use a union merge for this file,
 - so the lines may be in arbitrary order, but it will never conflict.
 -}

module LocationLog (
	LogStatus(..),
	logChange
) where

import Data.Time.Clock.POSIX
import Data.Time
import System.Locale
import qualified Data.Map as Map
import System.IO
import System.Directory
import Data.Char
import GitRepo
import Utility
import UUID
import Locations

data LogLine = LogLine {
	date :: POSIXTime,
	status :: LogStatus,
	uuid :: UUID
} deriving (Eq)

data LogStatus = FilePresent | FileMissing | Undefined
	deriving (Eq)

instance Show LogStatus where
	show FilePresent = "1"
	show FileMissing = "0"
	show Undefined = "undefined"

instance Read LogStatus where
	readsPrec _ "1" = [(FilePresent, "")]
	readsPrec _ "0" = [(FileMissing, "")]
	readsPrec _ _   = [(Undefined, "")]

instance Show LogLine where
	show (LogLine date status uuid) = unwords
		[(show date), (show status), uuid]

instance Read LogLine where
	-- This parser is robust in that even unparsable log lines are
	-- read without an exception being thrown.
	-- Such lines have a status of Undefined.
	readsPrec _ string = 
		if (length w >= 3)
			then case (pdate) of
				Just v -> good v
				Nothing -> undefined
			else undefined
		where
			w = words string
			date = w !! 0
			status = read $ w !! 1
			uuid = w !! 3
			pdate = (parseTime defaultTimeLocale "%s%Qs" date) :: Maybe UTCTime

			good v = ret $ LogLine (utcTimeToPOSIXSeconds v) status uuid
			undefined = ret $ LogLine (0) Undefined ""
			ret v = [(v, "")]

{- Log a change in the presence of a file in a repository,
 - and add the log to git so it will propigate to other repos. -}
logChange :: GitRepo -> FilePath -> UUID -> LogStatus -> IO ()
logChange repo file uuid status = do
	log <- logNow status uuid
	if (status == FilePresent)
		-- file added; just append to log
		then appendLog logfile log
		-- file removed; compact log
		else do
			ls <- readLog logfile
			writeLog logfile (log:ls)
	gitAdd repo logfile
	where
		logfile = logFile repo file

{- Reads a log file.
 - Note that the LogLines returned may be in any order. -}
readLog :: FilePath -> IO [LogLine]
readLog file = do
	exists <- doesFileExist file
	if exists
		then do
			s <- withFileLocked file ReadMode $ \h -> 
				hGetContentsStrict h
			-- filter out any unparsable lines
			return $ filter (\l -> (status l) /= Undefined )
				$ map read $ lines s
		else do
			return []

{- Adds a LogLine to a log file -}
appendLog :: FilePath -> LogLine -> IO ()
appendLog file line = do
	createDirectoryIfMissing True (parentDir file)
	withFileLocked file AppendMode $ \h ->
		hPutStrLn h $ show line
		-- TODO git add log

{- Writes a set of lines to a log file -}
writeLog :: FilePath -> [LogLine] -> IO ()
writeLog file lines = do
	createDirectoryIfMissing True (parentDir file)
	withFileLocked file WriteMode $ \h ->
		hPutStr h $ unlines $ map show lines

{- Generates a new LogLine with the current date. -}
logNow :: LogStatus -> UUID -> IO LogLine
logNow status uuid = do
	now <- getPOSIXTime
	return $ LogLine now status uuid

{- Returns the filename of the log file for a given annexed file. -}
logFile :: GitRepo -> FilePath -> String
logFile repo annexedFile = (gitStateDir repo) ++
		(gitRelative repo annexedFile) ++ ".log"

{- Returns a list of repository UUIDs that, according to the log, have
 - the content of a file -}
fileLocations :: GitRepo -> FilePath -> IO [UUID]
fileLocations thisrepo file = do
	lines <- readLog $ logFile thisrepo file
	return $ map uuid (filterPresent lines)

{- Filters the list of LogLines to find ones where the file
 - is (or should still be) present. -}
filterPresent :: [LogLine] -> [LogLine]
filterPresent lines = filter (\l -> FilePresent == status l) $ compactLog lines

{- Compacts a set of logs, returning a subset that contains the current
 - status. -}
compactLog :: [LogLine] -> [LogLine]
compactLog lines = compactLog' Map.empty lines
compactLog' map [] = Map.elems map
compactLog' map (l:ls) = compactLog' (mapLog map l) ls

{- Inserts a log into a map of logs, if the log has better (ie, newer)
 - information about a repo than the other logs in the map -}
mapLog map log = 
	if (better)
		then Map.insert (uuid log) log map
		else map
	where
		better = case (Map.lookup (uuid log) map) of
			Just l -> (date l <= date log)
			Nothing -> True
