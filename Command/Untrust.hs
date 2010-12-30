{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Untrust where

import Control.Monad.State (liftIO)
import Control.Monad (when)

import Command
import qualified Annex
import qualified GitRepo as Git
import qualified Remotes
import UUID
import Messages

command :: [Command]
command = [Command "untrust" (paramRepeating paramRemote) seek
	"do not trust a repository"]

seek :: [CommandSeek]
seek = [withString start]

{- Marks a remote as not trusted. -}
start :: CommandStartString
start name = do
	r <- Remotes.byName name
	showStart "untrust" name
	return $ Just $ perform r

perform :: Git.Repo -> CommandPerform
perform repo = do
	uuid <- getUUID repo
	trusted <- getTrusted
	when (elem uuid trusted) $ do
		setTrusted $ filter (\u -> u /= uuid) trusted
		g <- Annex.gitRepo
		logfile <- trustLog
		liftIO $ Git.run g ["add", logfile]
		liftIO $ Git.run g ["commit", "-m", "git annex untrust", logfile]
	return $ Just $ return True
