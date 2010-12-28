{- git-annex utility functions
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility (
	hGetContentsStrict,
	readFileStrict,
	parentDir,
	absPath,
	relPathCwdToDir,
	relPathDirToDir,
	boolSystem,
	shellEscape,
	unsetFileMode,
	readMaybe,
	safeWriteFile
) where

import System.IO
import System.Exit
import System.Posix.Process
import System.Posix.Signals
import System.Posix.Files
import System.Posix.Types
import Data.String.Utils
import System.Path
import System.FilePath
import System.Directory
import Foreign (complement)

{- A version of hgetContents that is not lazy. Ensures file is 
 - all read before it gets closed. -}
hGetContentsStrict :: Handle -> IO String
hGetContentsStrict h  = hGetContents h >>= \s -> length s `seq` return s

{- A version of readFile that is not lazy. -}
readFileStrict :: FilePath -> IO String
readFileStrict f = readFile f >>= \s -> length s `seq` return s

{- Returns the parent directory of a path. Parent of / is "" -}
parentDir :: String -> String
parentDir dir =
	if not $ null dirs
	then slash ++ join s (take (length dirs - 1) dirs)
	else ""
		where
			dirs = filter (not . null) $ split s dir
			slash = if isAbsolute dir then s else ""
			s = [pathSeparator]

{- Converts a filename into a normalized, absolute path. -}
absPath :: FilePath -> IO FilePath
absPath file = do
	cwd <- getCurrentDirectory
	case absNormPath cwd file of
		Just f -> return f
		Nothing -> error $ "unable to normalize " ++ file

{- Constructs a relative path from the CWD to a directory.
 -
 - For example, assuming CWD is /tmp/foo/bar:
 -    relPathCwdToDir "/tmp/foo" == "../"
 -    relPathCwdToDir "/tmp/foo/bar" == "" 
 -}
relPathCwdToDir :: FilePath -> IO FilePath
relPathCwdToDir dir = do
	cwd <- getCurrentDirectory
	a <- absPath dir
	return $ relPathDirToDir cwd a

{- Constructs a relative path from one directory to another.
 -
 - Both directories must be absolute, and normalized (eg with absNormpath).
 -
 - The path will end with "/", unless it is empty.
 -}
relPathDirToDir :: FilePath -> FilePath -> FilePath
relPathDirToDir from to = 
	if not $ null path
		then addTrailingPathSeparator path
		else ""
	where
		s = [pathSeparator]
		pfrom = split s from
		pto = split s to
		common = map fst $ filter same $ zip pfrom pto
		same (c,d) = c == d
		uncommon = drop numcommon pto
		dotdots = replicate (length pfrom - numcommon) ".."
		numcommon = length common
		path = join s $ dotdots ++ uncommon

{- Run a system command, and returns True or False
 - if it succeeded or failed.
 -
 - SIGINT(ctrl-c) is allowed to propigate and will terminate the program.
 -}
boolSystem :: FilePath -> [String] -> IO Bool
boolSystem command params = do
	-- Going low-level because all the high-level system functions
	-- block SIGINT etc. We need to block SIGCHLD, but allow
	-- SIGINT to do its default program termination.
	let sigset = addSignal sigCHLD emptySignalSet
	oldint <- installHandler sigINT Default Nothing
	oldset <- getSignalMask
	blockSignals sigset
	childpid <- forkProcess $ childaction oldint oldset
	mps <- getProcessStatus True False childpid
	restoresignals oldint oldset
	case mps of
		Just (Exited ExitSuccess) -> return True
		_ -> return False
	where
		restoresignals oldint oldset = do
			_ <- installHandler sigINT oldint Nothing
			setSignalMask oldset
		childaction oldint oldset = do
			restoresignals oldint oldset
			executeFile command True params Nothing

{- Escapes a filename to be safely able to be exposed to the shell. -}
shellEscape :: FilePath -> String
shellEscape f = "'" ++ escaped ++ "'"
	where
		-- replace ' with '"'"'
		escaped = join "'\"'\"'" $ split "'" f

{- Removes a FileMode from a file.
 - For example, call with otherWriteMode to chmod o-w -}
unsetFileMode :: FilePath -> FileMode -> IO ()
unsetFileMode f m = do
	s <- getFileStatus f
	setFileMode f $ fileMode s `intersectFileModes` complement m

{- Attempts to read a value from a String. -}
readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
	((x,_):_) -> Just x
	_ -> Nothing

{- Writes a file using a temp file that is renamed atomically into place. -}
safeWriteFile :: FilePath -> String -> IO ()
safeWriteFile file content = do
	pid <- getProcessID
        let tmpfile = file ++ ".tmp" ++ show pid
	createDirectoryIfMissing True (parentDir file)
	writeFile tmpfile content
	renameFile tmpfile file
