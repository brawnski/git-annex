{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Uninit where

import Control.Monad.State (liftIO)
import Control.Monad (when)
import System.Directory

import Command
import Messages
import Types
import Utility
import qualified GitRepo as Git
import qualified Annex
import qualified Command.Unannex
import qualified Command.Init

command :: [Command]
command = [Command "uninit" paramPath seek 
        "de-initialize git-annex and clean out repository"]

seek :: [CommandSeek]
seek = [withFilesInGit Command.Unannex.start, withNothing start]

start :: CommandStartNothing
start = do
	showStart "uninit" ""
	return $ Just $ perform

perform :: CommandPerform
perform = do
	g <- Annex.gitRepo

	gitPreCommitHookUnWrite g
	liftIO $ gitAttributesUnWrite g

	return $ Just $ return True

gitPreCommitHookUnWrite :: Git.Repo -> Annex ()
gitPreCommitHookUnWrite repo = do
	let hook = Command.Init.preCommitHook repo
	hookexists <- liftIO $ doesFileExist hook
	when hookexists $ do
		c <- liftIO $ readFile hook
		if c == Command.Init.preCommitScript
			then liftIO $ removeFile hook
			else warning $ "pre-commit hook (" ++ hook ++ 
				") contents modified; not deleting." ++
				" Edit it to remove call to git annex."

gitAttributesUnWrite :: Git.Repo -> IO ()
gitAttributesUnWrite repo = do
	let attributes = Git.attributes repo
	attrexists <- doesFileExist attributes
	when attrexists $ do
		c <- readFileStrict attributes
		safeWriteFile attributes $ unlines $
			filter (\l -> not $ l `elem` Command.Init.attrLines) $ lines c
