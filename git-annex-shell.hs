{- git-annex-shell main program
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

import System.Environment
import Control.Monad (when)

import qualified GitRepo as Git
import CmdLine
import Command
import Utility
import Options

import qualified Command.ConfigList
import qualified Command.InAnnex
import qualified Command.DropKey
import qualified Command.RecvKey
import qualified Command.SendKey

cmds :: [Command]
cmds = map adddirparam $ concat
	[ Command.ConfigList.command
	, Command.InAnnex.command
	, Command.DropKey.command
	, Command.RecvKey.command
	, Command.SendKey.command
	]
	where
		adddirparam c = c { cmdparams = "DIRECTORY " ++ cmdparams c }

header :: String
header = "Usage: git-annex-shell [-c] command [option ..]"

main :: IO ()
main = do
	args <- getArgs
	main' args

main' :: [String] -> IO ()
main' [] = failure
-- skip leading -c options, passed by eg, ssh
main' ("-c":p) = main' p
-- Since git-annex explicitly runs git-annex-shell, we will be passed 
-- a redundant "git-annex-shell" parameter when we're the user's login shell.
main' ("git-annex-shell":p) = main' p
-- a command can be either a builtin or something to pass to git-shell
main' c@(cmd:dir:params)
	| elem cmd builtins = builtin cmd dir params
	| otherwise = external c
main' c@(cmd:_)
	| elem cmd builtins = failure
	| otherwise = external c

builtins :: [String]
builtins = map cmdname cmds

builtin :: String -> String -> [String] -> IO ()
builtin cmd dir params = do
	let gitrepo = Git.repoFromPath dir
	dispatch gitrepo (cmd:params) cmds commonOptions header

external :: [String] -> IO ()
external l = do
	ret <- boolSystem "git-shell" ("-c":l)
	when (not ret) $
		error "git-shell failed"

failure :: IO ()
failure = error $ "bad parameters\n\n" ++ usage header cmds commonOptions
