{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Trust where

import Control.Monad.State (liftIO)
import Control.Monad (unless)

import Command
import qualified Annex
import qualified GitRepo as Git
import qualified Remotes
import UUID
import Messages

command :: [Command]
command = [Command "trust" (paramRepeating paramRemote) seek
	"trust a repository"]

seek :: [CommandSeek]
seek = [withString start]

{- Marks a remote as trusted. -}
start :: CommandStartString
start name = do
	r <- Remotes.byName name
	showStart "trust" name
	return $ Just $ perform r

perform :: Git.Repo -> CommandPerform
perform repo = do
	uuid <- getUUID repo
	trusted <- getTrusted
	unless (elem uuid trusted) $ do
		setTrusted $ uuid:trusted
		g <- Annex.gitRepo
		logfile <- trustLog
		liftIO $ Git.run g ["add", logfile]
		liftIO $ Git.run g ["commit", "-q", "-m", "git annex untrust", logfile]
	return $ Just $ return True
