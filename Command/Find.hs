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

command :: [Command]
command = [Command "find" (paramOptional $ paramRepeating paramPath) seek
	"lists available files"]

seek :: [CommandSeek]
seek = [withDefault "." withFilesInGit start]

{- Output a list of files. -}
start :: CommandStartString
start file = isAnnexed file $ \(key, _) -> do
	exists <- inAnnex key
	when exists $ liftIO $ putStrLn file
	return Nothing
