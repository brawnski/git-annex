{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Find where

import Control.Monad (when)
import Control.Monad.State (liftIO)

import Command
import Core

seek :: [SubCmdSeek]
seek = [withDefault "." withFilesInGit start]

{- Output a list of files. -}
start :: SubCmdStartString
start file = isAnnexed file $ \(key, _) -> do
	exists <- inAnnex key
	when exists $ liftIO $ putStrLn file
	return Nothing
