{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.InAnnex where

import Control.Monad.State (liftIO)
import System.Exit

import Command
import Types
import Core
import qualified Backend

command :: [Command]
command = [Command "inannex" (paramRepeating paramKey) seek
		"checks if keys are present in the annex"]

seek :: [CommandSeek]
seek = [withKeys start]

start :: CommandStartString
start keyname = do
	backends <- Backend.list
	let key = genKey (head backends) keyname
	present <- inAnnex key
	if present
		then return Nothing
		else liftIO $ exitFailure
