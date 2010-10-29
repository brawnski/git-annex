{- git-annex utility functions
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility (
	withFileLocked,
	hGetContentsStrict,
	parentDir,
	relPathCwdToDir,
	relPathDirToDir,
	boolSystem,
	shellEscape
) where

import System.IO
import System.Cmd
import System.Exit
import System.Posix.Signals
import System.Posix.IO
import Data.String.Utils
import System.Path
import System.IO.HVFS
import System.FilePath
import System.Directory

{- Let's just say that Haskell makes reading/writing a file with
 - file locking excessively difficult. -}
withFileLocked file mode action = do
	-- TODO: find a way to use bracket here
	handle <- openFile file mode
	lockfd <- handleToFd handle -- closes handle
	waitToSetLock lockfd (lockType mode, AbsoluteSeek, 0, 0)
	handle' <- fdToHandle lockfd
	ret <- action handle'
	hClose handle'
	return ret
		where
			lockType ReadMode = ReadLock
			lockType _ = WriteLock

{- A version of hgetContents that is not lazy. Ensures file is 
 - all read before it gets closed. -}
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
	let absdir = abs cwd dir
	return $ relPathDirToDir cwd absdir
	where
		-- absolute, normalized form of the directory
		abs cwd dir = 
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
 - An error is thrown if the command exits due to SIGINT,
 - to propigate ctrl-c.
 -}
boolSystem :: FilePath -> [String] -> IO Bool
boolSystem command params = do
	r <- rawSystem command params
	case r of
		ExitSuccess -> return True
		ExitFailure e -> if toInteger e == toInteger sigINT
			then error $ command ++ "interrupted"
			else return False

{- Escapes a filename to be safely able to be exposed to the shell. -}
shellEscape f = "'" ++ quote ++ "'"
	where
		-- replace ' with '"'"'
		quote = join "'\"'\"'" $ split "'" f
