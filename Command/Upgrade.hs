{- git-annex command
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Upgrade where

import Command
import Upgrade
import Version

command :: [Command]
command = [standaloneCommand "upgrade" paramNothing seek
	"upgrade repository layout"]

seek :: [CommandSeek]
seek = [withNothing start]

start :: CommandStartNothing
start = do
	r <- upgrade
	checkVersion
	return $ Just $ return $ Just $ return r
