{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.SendKey where

import Control.Monad (when)
import Control.Monad.State (liftIO)
import System.Exit

import Locations
import qualified Annex
import Command
import Content
import RsyncFile

command :: [Command]
command = [repoCommand "sendkey" paramKey seek
	"runs rsync in server mode to send content"]

seek :: [CommandSeek]
seek = [withKeys start]

start :: CommandStartKey
start key = do
	present <- inAnnex key
	g <- Annex.gitRepo
	let file = gitAnnexLocation g key
	when present $
		liftIO $ rsyncServerSend file
	liftIO exitFailure
