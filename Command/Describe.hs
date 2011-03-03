{- git-annex command
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Describe where


import Command
import qualified GitRepo as Git
import qualified Remotes
import UUID
import Messages
import qualified Command.Init

command :: [Command]
command = [Command "describe" (paramPair paramRemote paramDesc) seek
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
	Remotes.readConfigs
	r <- Remotes.byName name
	return $ Just $ perform r description

perform :: Git.Repo -> String -> CommandPerform
perform repo description = do
	u <- getUUID repo
	describeUUID u description
	return $ Just $ Command.Init.cleanup
