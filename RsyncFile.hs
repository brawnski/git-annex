{- git-annex file copying with rsync
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module RsyncFile where

import System.Posix.Process
import Data.String.Utils

import Utility

{- Generates parameters to make rsync use a specified command as its remote
 - shell. -}
rsyncShell :: [ShellParam] -> [ShellParam]
rsyncShell command = [Param "-e", Param $ unwords $ map escape (toShell command)]
	where
		{- rsync requires some weird, non-shell like quoting in
                 - here. A doubled single quote inside the single quoted
                 - string is a single quote. -}
		escape s = "'" ++  (join "''" $ split "'" s) ++ "'"

{- Runs rsync in server mode to send a file, and exits. -}
rsyncServerSend :: FilePath -> IO ()
rsyncServerSend file = rsyncExec $
	rsyncServerParams ++ [Param "--sender", File file]

{- Runs rsync in server mode to receive a file. -}
rsyncServerReceive :: FilePath -> IO Bool
rsyncServerReceive file = rsync $ rsyncServerParams ++ [File file]

rsyncServerParams :: [ShellParam]
rsyncServerParams =
	[ Param "--server"
	-- preserve permissions
	, Param "-p"
	-- allow resuming of transfers of big files
	, Param "--inplace"
	-- other options rsync normally uses in server mode
	, Params "-e.Lsf ."
	]

rsync :: [ShellParam] -> IO Bool
rsync = boolSystem "rsync"

rsyncExec :: [ShellParam] -> IO ()
rsyncExec params = executeFile "rsync" True (toShell params) Nothing
