{- git-annex file copying
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module CopyFile (copyFile) where

import Control.Monad (when)
import System.Directory (doesFileExist, removeFile)

import Utility
import qualified SysConfig

{- The cp command is used, because I hate reinventing the wheel,
 - and because this allows easy access to features like cp --reflink. -}
copyFile :: FilePath -> FilePath -> IO Bool
copyFile src dest = do
	e <- doesFileExist dest
	when e $
		removeFile dest
	boolSystem "cp" opts
	where
		opts = if SysConfig.cp_reflink_auto
			then ["--reflink=auto", src, dest]
			else if SysConfig.cp_a
				then ["-a", src, dest]
				else if SysConfig.cp_p
					then ["-p", src, dest]
					else [src, dest]
