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
rsyncShell :: [String] -> [String]
rsyncShell command = ["-e", unwords $ map escape command]
	where
		{- rsync requires some weird, non-shell like quoting in
                 - here. A doubled single quote inside the single quoted
                 - string is a single quote. -}
		escape s = "'" ++  (join "''" $ split "'" s) ++ "'"

{- Runs rsync in server mode to send a file, and exits. -}
rsyncServerSend :: FilePath -> IO ()
rsyncServerSend file = rsyncExec $
	rsyncServerParams ++ ["--sender", utilityEscape file]

{- Runs rsync in server mode to receive a file. -}
rsyncServerReceive :: FilePath -> IO Bool
rsyncServerReceive file = rsync $ rsyncServerParams ++ [utilityEscape file]

rsyncServerParams :: [String]
rsyncServerParams =
	[ "--server"
	, "-p"		-- preserve permissions
	, "--inplace"	-- allow resuming of transfers of big files
	, "-e.Lsf", "."	-- other options rsync normally uses in server mode
	]

rsync :: [String] -> IO Bool
rsync params = boolSystem "rsync" params 

rsyncExec :: [String] -> IO ()
rsyncExec params = executeFile "rsync" True params Nothing
