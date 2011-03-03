{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Trust where

import Command
import qualified GitRepo as Git
import qualified Remotes
import Trust
import UUID
import Messages

command :: [Command]
command = [Command "trust" (paramRepeating paramRemote) seek
	"trust a repository"]

seek :: [CommandSeek]
seek = [withString start]

start :: CommandStartString
start name = notBareRepo $ do
	showStart "trust" name
	Remotes.readConfigs
	r <- Remotes.byName name
	return $ Just $ perform r

perform :: Git.Repo -> CommandPerform
perform repo = do
	uuid <- getUUID repo
	trustSet uuid Trusted
	return $ Just $ return True
