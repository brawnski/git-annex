{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.RecvKey where

import Control.Monad (when)
import Control.Monad.State (liftIO)
import System.Exit

import Command
import Types
import CmdLine
import Content
import qualified Backend
import RsyncFile

command :: [Command]
command = [Command "recvkey" paramKey seek
	"runs rsync in server mode to receive content"]

seek :: [CommandSeek]
seek = [withKeys start]

start :: CommandStartString
start keyname = do
	error "BROKEN FIXME!"
	{-
	backends <- Backend.list
	let key = genKey (head backends) keyname
	present <- inAnnex key
	when present $
		error "key is already present in annex"
	
	ok <- getViaTmp key (liftIO . rsyncServerReceive)
	if ok
		then do
			-- forcibly quit after receiving one key,
			-- and shutdown cleanly so queued git commands run
			_ <- shutdown
			liftIO exitSuccess
		else liftIO exitFailure
	-}
