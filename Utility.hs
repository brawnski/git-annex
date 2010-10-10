{- git-annex utility functions
 -}

module Utility where

import System.IO
import System.Posix.IO
import Data.String.Utils

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

{- Returns the parent directory of a path. Parent of / is "" -}
parentDir :: String -> String
parentDir dir =
	if length dirs > 0
	then "/" ++ (join "/" $ take ((length dirs) - 1) dirs)
	else ""
		where
			dirs = filter (\x -> length x > 0) $ split "/" dir
