{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Init where

import Control.Monad.State (liftIO)
import Control.Monad (when)
import System.Directory

import Command
import qualified Annex
import qualified GitRepo as Git
import UUID
import Version
import Messages
import Locations
import Types

seek :: [SubCmdSeek]
seek = [withString start]

{- Stores description for the repository etc. -}
start :: SubCmdStartString
start description = do
	when (null description) $
		error "please specify a description of this repository\n"
	showStart "init" description
	return $ Just $ perform description

perform :: String -> SubCmdPerform
perform description = do
	g <- Annex.gitRepo
	u <- getUUID g
	describeUUID u description
	setVersion
	liftIO $ gitAttributesWrite g
	gitPreCommitHookWrite g
	return $ Just cleanup

cleanup :: SubCmdCleanup
cleanup = do
	g <- Annex.gitRepo
	logfile <- uuidLog
	liftIO $ Git.run g ["add", logfile]
	liftIO $ Git.run g ["commit", "-m", "git annex init", logfile]
	return True

{- configure git to use union merge driver on state files, if it is not
 - already -}
gitAttributesWrite :: Git.Repo -> IO ()
gitAttributesWrite repo = do
	exists <- doesFileExist attributes
	if not exists
		then do
			writeFile attributes $ attrLine ++ "\n"
			commit
		else do
			content <- readFile attributes
			when (all (/= attrLine) (lines content)) $ do
				appendFile attributes $ attrLine ++ "\n"
				commit
	where
		attributes = Git.attributes repo
		commit = do
			Git.run repo ["add", attributes]
			Git.run repo ["commit", "-m", "git-annex setup", 
					attributes]

attrLine :: String		
attrLine = stateLoc ++ "*.log merge=union"

{- set up a git pre-commit hook, if one is not already present -}
gitPreCommitHookWrite :: Git.Repo -> Annex ()
gitPreCommitHookWrite repo = do
	exists <- liftIO $ doesFileExist hook
	if exists
		then warning $ "pre-commit hook (" ++ hook ++ ") already exists, not configuring"
		else liftIO $ do
			writeFile hook preCommitScript
			p <- getPermissions hook
			setPermissions hook $ p {executable = True}
	where
		hook = preCommitHook repo

preCommitHook :: Git.Repo -> FilePath
preCommitHook repo = 
	Git.workTree repo ++ "/" ++ Git.gitDir repo ++ "/hooks/pre-commit"

preCommitScript :: String
preCommitScript = 
		"#!/bin/sh\n" ++
		"# automatically configured by git-annex\n" ++ 
		"git annex pre-commit .\n"
