{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Untrust where

import Command
import qualified GitRepo as Git
import qualified Remotes
import UUID
import Trust
import Messages

command :: [Command]
command = [Command "untrust" (paramRepeating paramRemote) seek
	"do not trust a repository"]

seek :: [CommandSeek]
seek = [withString start]

start :: CommandStartString
start name = do
	showStart "untrust" name
	Remotes.readConfigs
	r <- Remotes.byName name
	return $ Just $ perform r

perform :: Git.Repo -> CommandPerform
perform repo = do
	uuid <- getUUID repo
	trustSet uuid UnTrusted
	return $ Just $ return True
