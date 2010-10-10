{- git-annex location log
 -
 - git-annex keeps track of on which repository it last saw a file's content.
 - This can be useful when using it for archiving with offline storage.
 - When you indicate you --want a file, git-annex will tell you which
 - repositories have the file's content.
 -
 - Location tracking information is stored in `.git-annex/filename.log`.
 - Repositories record their name and the date when they --get or --drop
 - a file's content. (Git is configured to use a union merge for this file,
 - so the lines may be in arbitrary order, but it will never conflict.)
 -
 - A line of the log will look like: "date N reponame"
 - Where N=1 when the repo has the file, and 0 otherwise.
 -
 -}

module LocationLog where

import Data.DateTime
import System.IO
import System.Directory
import Data.Char
import GitRepo
import Utility

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

data LogLine = LogLine {
	date :: DateTime,
	status :: LogStatus,
	repo :: String
} deriving (Eq)

instance Show LogLine where
	show (LogLine date status repo) = unwords
		[(show (toSeconds date)), (show status), repo]

instance Read LogLine where
	-- This parser is robust in that even unparsable log lines are
	-- read without an exception being thrown.
	-- Such lines have a status of Undefined.
	readsPrec _ string = 
		if (length w >= 3 && all isDigit date)
			then [((LogLine (fromSeconds $ read date) status repo), "")]
			else [((LogLine (fromSeconds 0) Undefined ""), "")]
		where
			w = words string
			date = w !! 0
			status = read $ w !! 1
			repo = unwords $ drop 2 w

{- Reads a log file.
 - Note that the LogLines returned may be in any order. -}
readLog :: String -> IO [LogLine]
readLog file = do
	exists <- doesFileExist file
	if exists
		then do
			h <- openLocked file ReadMode
			s <- hGetContents h
			-- hClose handle' -- TODO disabled due to lazy IO issue
			-- filter out any unparsable lines
			return $ filter (\l -> (status l) /= Undefined )
				$ map read $ lines s
		else do
			return []

{- Adds a LogLine to a log file -}
writeLog :: String -> LogLine -> IO ()
writeLog file line = do
	createDirectoryIfMissing True (parentDir file)
	h <- openLocked file AppendMode
	hPutStrLn h $ show line
	hClose h

{- Generates a new LogLine with the current date. -}
logNow :: LogStatus -> String -> IO LogLine
logNow status repo = do
	now <- getCurrentTime
	return $ LogLine now status repo

{- Returns the filename of the log file for a given annexed file. -}
logFile :: String -> IO String
logFile annexedFile = do
	repo <- repoTop
	return $ repo ++ "/.git-annex/" ++ 
		(gitRelative repo annexedFile) ++ ".log"

{- Returns a list of repositories that, according to the log, have
 - the content of a file -}
fileLocations :: String -> IO [String]
fileLocations file = do
	log <- logFile file
	lines <- readLog log
	return $ map repo (filterPresent lines)

{- Filters the list of LogLines to find repositories where the file
 - is (or should still be) present. -}
filterPresent :: [LogLine] -> [LogLine]
filterPresent lines = error "unimplimented" -- TODO
