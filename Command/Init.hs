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
import System.FilePath

import Command
import qualified Annex
import qualified GitRepo as Git
import UUID
import Version
import Messages
import Locations
import Types
import Utility
	
command :: [Command]
command = [Command "init" paramDesc seek
		"initialize git-annex with repository description"]

seek :: [CommandSeek]
seek = [withString start]

{- Stores description for the repository etc. -}
start :: CommandStartString
start description = do
	when (null description) $
		error "please specify a description of this repository\n"
	showStart "init" description
	return $ Just $ perform description

perform :: String -> CommandPerform
perform description = do
	g <- Annex.gitRepo
	u <- getUUID g
	setVersion
	if Git.repoIsLocalBare g
		then do
			showLongNote $
				"This is a bare repository, so its description cannot be committed.\n" ++
				"To record the description, run this command in a clone of this repository:\n" ++
				"   git annex describe " ++ (show u) ++ " '" ++ description ++ "'\n\n"
			return $ Just $ return True
		else do
			describeUUID u description
			liftIO $ gitAttributesWrite g
			gitPreCommitHookWrite g
			return $ Just cleanup

cleanup :: CommandCleanup
cleanup = do
	g <- Annex.gitRepo
	logfile <- uuidLog
	liftIO $ Git.run g "add" [File logfile]
	liftIO $ Git.run g "commit" 
		[ Params "-q -m"
		, Param "git annex init"
		, File logfile
		]
	return True

{- configure git to use union merge driver on state files, if it is not
 - already -}
gitAttributesWrite :: Git.Repo -> IO ()
gitAttributesWrite repo = do
	exists <- doesFileExist attributes
	if not exists
		then do
			safeWriteFile attributes $ attrLine ++ "\n"
			commit
		else do
			content <- readFile attributes
			when (all (/= attrLine) (lines content)) $ do
				appendFile attributes $ attrLine ++ "\n"
				commit
	where
		attributes = Git.attributes repo
		commit = do
			Git.run repo "add" [Param attributes]
			Git.run repo "commit"
				[ Params "-q -m"
				, Param "git-annex setup"
				, Param attributes
				]

attrLine :: String		
attrLine = stateDir </> "*.log merge=union"

{- set up a git pre-commit hook, if one is not already present -}
gitPreCommitHookWrite :: Git.Repo -> Annex ()
gitPreCommitHookWrite repo = do
	exists <- liftIO $ doesFileExist hook
	if exists
		then warning $ "pre-commit hook (" ++ hook ++ ") already exists, not configuring"
		else liftIO $ do
			safeWriteFile hook preCommitScript
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
