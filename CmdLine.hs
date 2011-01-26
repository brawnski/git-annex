{- git-annex command line parsing and dispatch
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module CmdLine (
	dispatch,
	usage,
	shutdown
) where

import System.IO.Error (try)
import System.Console.GetOpt
import Control.Monad.State (liftIO)
import Control.Monad (when, unless)
import System.Directory

import qualified Annex
import qualified GitRepo as Git
import qualified GitQueue
import Types
import Command
import BackendList
import Upgrade
import Options
import Messages
import UUID
import Locations

{- Runs the passed command line. -}
dispatch :: Git.Repo -> [String] -> [Command] -> [Option] -> String -> IO ()
dispatch gitrepo args cmds options header = do
	state <- Annex.new gitrepo allBackends
	(actions, state') <- Annex.run state $ parseCmd args header cmds options
	tryRun state' $ [startup, upgrade] ++ actions

{- Parses command line, stores configure flags, and returns a 
 - list of actions to be run in the Annex monad. -}
parseCmd :: [String] -> String -> [Command] -> [Option] -> Annex [Annex Bool]
parseCmd argv header cmds options = do
	(flags, params) <- liftIO $ getopt
	when (null params) $ error $ "missing command" ++ usagemsg
	case lookupCmd (head params) of
		[] -> error $ "unknown command" ++ usagemsg
		[command] -> do
			_ <- sequence flags
			prepCmd command (drop 1 params)
		_ -> error "internal error: multiple matching commands"
	where
		getopt = case getOpt Permute options argv of
			(flags, params, []) ->
				return (flags, params)
			(_, _, errs) ->
				ioError (userError (concat errs ++ usagemsg))
		lookupCmd cmd = filter (\c -> cmd  == cmdname c) cmds
		usagemsg = "\n\n" ++ usage header cmds options

{- Usage message with lists of commands and options. -}
usage :: String -> [Command] -> [Option] -> String
usage header cmds options =
	usageInfo (header ++ "\n\nOptions:") options ++
		"\nCommands:\n" ++ cmddescs
	where
		cmddescs = unlines $ map (indent . showcmd) cmds
		showcmd c =
			cmdname c ++
			pad (longest cmdname + 1) (cmdname c) ++
			cmdparams c ++
			pad (longest cmdparams + 2) (cmdparams c) ++
			cmddesc c
		indent l = "  " ++ l
		pad n s = replicate (n - length s) ' '
		longest f = foldl max 0 $ map (length . f) cmds

{- Runs a list of Annex actions. Catches IO errors and continues
 - (but explicitly thrown errors terminate the whole command).
 - Runs shutdown and propigates an overall error status at the end.
 -}
tryRun :: Annex.AnnexState -> [Annex Bool] -> IO ()
tryRun state actions = tryRun' state 0 actions
tryRun' :: Annex.AnnexState -> Integer -> [Annex Bool] -> IO ()
tryRun' state errnum (a:as) = do
	result <- try $ Annex.run state a
	case result of
		Left err -> do
			Annex.eval state $ showErr err
			tryRun' state (errnum + 1) as
		Right (True,state') -> tryRun' state' errnum as
		Right (False,state') -> tryRun' state' (errnum + 1) as
tryRun' state errnum [] = do
	_ <- try $ Annex.run state $ shutdown errnum
	when (errnum > 0) $ error $ show errnum ++ " failed"

{- Actions to perform each time ran. -}
startup :: Annex Bool
startup = do
	prepUUID
	return True

{- Cleanup actions. -}
shutdown :: Integer -> Annex ()
shutdown errnum = do
	q <- Annex.queueGet
	unless (q == GitQueue.empty) $ do
		showSideAction "Recording state in git..."
		Annex.queueRun

	-- If nothing failed, clean up any files left in the temp directory,
	-- but leave the directory itself. If something failed, temp files
	-- are left behind to allow resuming on re-run.
	when (errnum == 0) $ do
		g <- Annex.gitRepo
		let tmp = annexTmpLocation g
		exists <- liftIO $ doesDirectoryExist tmp
		when exists $ liftIO $ removeDirectoryRecursive tmp
		liftIO $ createDirectoryIfMissing True tmp
