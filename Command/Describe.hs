{- git-annex command
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Describe where

import Command
import qualified Remote
import UUID
import Messages
import qualified Command.Init

command :: [Command]
command = [repoCommand "describe" (paramPair paramRemote paramDesc) seek
	"change description of a repository"]

seek :: [CommandSeek]
seek = [withString start]

start :: CommandStartString
start params = notBareRepo $ do
	let (name, description) =
		case (words params) of
			(n:d) -> (n,unwords d)
			_ -> error "Specify a repository and a description."
	
	showStart "describe" name
	u <- Remote.nameToUUID name
	return $ Just $ perform u description

perform :: UUID -> String -> CommandPerform
perform u description = do
	describeUUID u description
	return $ Just $ Command.Init.cleanup
