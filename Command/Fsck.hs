{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Fsck where

import Control.Monad.State (liftIO)
import System.Posix.Files
import System.Directory

import Command
import qualified Annex
import Types
import Utility
import Core

{- Checks the whole annex for problems. -}
start :: SubCmdStart
start = do
	showStart "fsck" ""
	return $ Just perform

perform :: SubCmdPerform
perform = do
	ok <- checkUnused
	if (ok)
		then return $ Just $ return True
		else do
			showLongNote "Possible problems detected."
			return Nothing

checkUnused :: Annex Bool
checkUnused = do
	showNote "checking for unused data..."
	-- TODO
	return False
