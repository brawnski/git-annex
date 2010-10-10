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
 - A line of the log will look like: "date reponame filename"
 -
 -}

module LocationLog where

import Data.DateTime
import System.IO
import System.Posix.IO
import GitRepo

data LogLine = LogLine {
	date :: DateTime,
	repo :: String,
	file :: String
} deriving (Eq)

-- a special value representing a log file line that could not be parsed
unparsable = (LogLine (fromSeconds 0) "" "")

instance Show LogLine where
	show (LogLine date repo file) = unwords
		[(show (toSeconds date)), repo, file]

instance Read LogLine where
	-- this parser is robust in that even unparsable log lines are
	-- read without an exception being thrown
	readsPrec _ string = if (length w >= 3)
				then [((LogLine time repo file), "")]
				else [(unparsable, "")]
		where
			time = fromSeconds $ read $ w !! 0
			repo = w !! 1
			file = unwords $ rest w
			w = words string
			rest (_:_:l) = l

{- Reads a log file -}
readLog :: String -> IO [LogLine]
readLog file = do
	h <- openLocked file ReadMode
	s <- hGetContents h
	-- hClose handle' -- TODO disabled due to lazy IO issue
	-- filter out any unparsable lines
	return $ filter ( /= unparsable ) $ map read $ lines s

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
logNow :: String -> String -> IO LogLine
logNow repo file = do
	now <- getCurrentTime
	return $ LogLine now repo file

{- Returns the filename of the log file for a given annexed file. -}
logFile :: String -> IO String
logFile annexedFile = do
	repo <- repoTop
	return $ repo ++ "/.git-annex/" ++ 
		(gitRelative repo annexedFile) ++ ".log"
