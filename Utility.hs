{- git-annex utility functions
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility (
	hGetContentsStrict,
	parentDir,
	relPathCwdToDir,
	relPathDirToDir,
	boolSystem,
	shellEscape,
	unsetFileMode
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

{- Returns the parent directory of a path. Parent of / is "" -}
parentDir :: String -> String
parentDir dir =
	if (not $ null dirs)
	then slash ++ (join s $ take ((length dirs) - 1) dirs)
	else ""
		where
			dirs = filter (\x -> length x > 0) $ 
				split s dir
			slash = if (not $ isAbsolute dir) then "" else s
			s = [pathSeparator]

{- Constructs a relative path from the CWD to a directory.
 -
 - For example, assuming CWD is /tmp/foo/bar:
 -    relPathCwdToDir "/tmp/foo" == "../"
 -    relPathCwdToDir "/tmp/foo/bar" == "" 
 -    relPathCwdToDir "/tmp/foo/bar" == "" 
 -}
relPathCwdToDir :: FilePath -> IO FilePath
relPathCwdToDir dir = do
	cwd <- getCurrentDirectory
	let absdir = absnorm cwd
	return $ relPathDirToDir cwd absdir
	where
		-- absolute, normalized form of the directory
		absnorm cwd = 
			case (absNormPath cwd dir) of
				Just d -> d
				Nothing -> error $ "unable to normalize " ++ dir

{- Constructs a relative path from one directory to another.
 -
 - Both directories must be absolute, and normalized (eg with absNormpath).
 -
 - The path will end with "/", unless it is empty.
 -}
relPathDirToDir :: FilePath -> FilePath -> FilePath
relPathDirToDir from to = 
	if (not $ null path)
		then addTrailingPathSeparator path
		else ""
	where
		s = [pathSeparator]
		pfrom = split s from
		pto = split s to
		common = map fst $ filter same $ zip pfrom pto
		same (c,d) = c == d
		uncommon = drop numcommon pto
		dotdots = take ((length pfrom) - numcommon) $ repeat ".."
		numcommon = length $ common
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
	setFileMode f $ (fileMode s) `intersectFileModes` (complement m)
