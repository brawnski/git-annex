{- git-annex utility functions
 -}

module Utility (
	withFileLocked,
	hGetContentsStrict,
	parentDir
) where

import System.IO
import System.Posix.IO
import Data.String.Utils

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
	if length dirs > 0
	then absolute ++ (join "/" $ take ((length dirs) - 1) dirs)
	else ""
		where
			dirs = filter (\x -> length x > 0) $ split "/" dir
			absolute = if ((dir !! 0) == '/') then "/" else ""
