{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Init where

import Control.Monad.State (liftIO)
import Control.Monad (when)

import Command
import qualified Annex
import Core
import qualified GitRepo as Git
import UUID

{- Stores description for the repository etc. -}
start :: SubCmdStartString
start description = do
	when (null description) $ error $
		"please specify a description of this repository\n"
	showStart "init" description
	return $ Just $ perform description

perform :: String -> SubCmdPerform
perform description = do
	g <- Annex.gitRepo
	u <- getUUID g
	describeUUID u description
	liftIO $ gitAttributes g
	liftIO $ gitPreCommitHook g
	return $ Just $ cleanup

cleanup :: SubCmdCleanup
cleanup = do
	g <- Annex.gitRepo
	logfile <- uuidLog
	liftIO $ Git.run g ["add", logfile]
	liftIO $ Git.run g ["commit", "-m", "git annex init", logfile]
	return True
