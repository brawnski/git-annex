{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Uninit where

import Control.Monad.State (liftIO)
import System.Directory
import System.Exit

import Command
import Messages
import Types
import Utility
import qualified Git
import qualified Annex
import qualified Command.Unannex
import qualified Command.Init
import qualified Branch
import Content
import Locations

command :: [Command]
command = [repoCommand "uninit" paramPath seek 
        "de-initialize git-annex and clean out repository"]

seek :: [CommandSeek]
seek = [withFilesInGit Command.Unannex.start, withNothing start]

start :: CommandStartNothing
start = next perform

perform :: CommandPerform
perform = next cleanup

cleanup :: CommandCleanup
cleanup = do
	g <- Annex.gitRepo
	gitPreCommitHookUnWrite g
	saveState
	liftIO $ do
		Git.run g "branch" [Param "-D", Param Branch.name]
		removeDirectoryRecursive (gitAnnexDir g)
		-- avoid normal shutdown
		exitSuccess

gitPreCommitHookUnWrite :: Git.Repo -> Annex ()
gitPreCommitHookUnWrite repo = do
	let hook = Command.Init.preCommitHook repo
	whenM (liftIO $ doesFileExist hook) $ do
		c <- liftIO $ readFile hook
		if c == Command.Init.preCommitScript
			then liftIO $ removeFile hook
			else warning $ "pre-commit hook (" ++ hook ++ 
				") contents modified; not deleting." ++
				" Edit it to remove call to git annex."
