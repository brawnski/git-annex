{- git-annex location log
 -
 - git-annex keeps track of on which repository it last saw a file's content.
 - This can be useful when using it for archiving with offline storage.
 - When you indicate you --want a file, git-annex will tell you which
 - repositories have the file's content.
 -
 - Location tracking information is stored in `.git-annex/$filename.log`.
 - Repositories record their name and the date when they --get or --drop
 - a file's content. (Git is configured to use a union merge for this file,
 - so the lines may be in arbitrary order, but it will never conflict.)
 -
 - A line of the log will look like: "date N reponame filename"
 - Where N=1 when the repo has the file, and 0 otherwise.
 -
 -}

module LocationLog where

import Data.DateTime
import System.IO
import System.Posix.IO
import GitRepo

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
	repo :: String,
	file :: String
} deriving (Eq)

instance Show LogLine where
	show (LogLine date status repo file) = unwords
		[(show (toSeconds date)), (show status), repo, file]

instance Read LogLine where
	-- This parser is robust in that even unparsable log lines are
	-- read without an exception being thrown.
	-- Such lines have a status of Undefined.
	readsPrec _ string = if (length w >= 3)
				then [((LogLine date status repo file), "")]
				else [((LogLine (fromSeconds 0) Undefined "" ""), "")]
		where
			date = fromSeconds $ read $ w !! 0
			status = read $ w !! 1
			repo = w !! 2
			file = unwords $ rest w
			w = words string
			rest (_:_:_:l) = l

{- Reads a log file -}
readLog :: String -> IO [LogLine]
readLog file = do
	h <- openLocked file ReadMode
	s <- hGetContents h
	-- hClose handle' -- TODO disabled due to lazy IO issue
	-- filter out any unparsable lines
	return $ filter (\l -> (status l) /= Undefined ) $ map read $ lines s

{- Adds a LogLine to a log file -}
writeLog :: String -> LogLine -> IO ()
writeLog file line = do
	h <- openLocked file AppendMode
	hPutStrLn h $ show line
	hClose h

{- Let's just say that Haskell makes reading/writing a file with
 - file locking excessively difficult. -}
openLocked file mode = do
	handle <- openFile file mode
	lockfd <- handleToFd handle -- closes handle
	waitToSetLock lockfd (lockType mode, AbsoluteSeek, 0, 0)
	handle' <- fdToHandle lockfd
	return handle'
		where
			lockType ReadMode = ReadLock
			lockType _ = WriteLock

{- Generates a new log line with the current date. -}
logNow :: LogStatus -> String -> String -> IO LogLine
logNow status repo file = do
	now <- getCurrentTime
	return $ LogLine now status repo file

{- Returns the filename of the log file for a given annexed file. -}
logFile :: String -> IO String
logFile annexedFile = do
	repo <- repoTop
	return $ repo ++ "/.git-annex/" ++ 
		(gitRelative repo annexedFile) ++ ".log"
