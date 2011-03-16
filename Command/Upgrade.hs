{- git-annex command
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Upgrade where

import Command

command :: [Command]
command = [Command "upgrade" paramNothing seek "upgrade repository layout"]

seek :: [CommandSeek]
seek = [withNothing start]

start :: CommandStartNothing
start = do
	-- The actual upgrading is handled by just running any command,
	-- so nothing extra needs to be done.
	return $ Just $ return $ Just $ return True
