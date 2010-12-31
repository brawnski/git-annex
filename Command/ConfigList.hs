{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.ConfigList where

import Control.Monad.State (liftIO)

import Annex
import Command
import qualified GitRepo as Git

command :: [Command]
command = [Command "configlist" paramNothing seek
		"outputs relevant git configuration"]

seek :: [CommandSeek]
seek = [withNothing start]

start :: CommandStartNothing
start = do
	g <- Annex.gitRepo
	liftIO $ Git.run g ["config", "--list"]
	return Nothing
