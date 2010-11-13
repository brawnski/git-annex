{- git-annex command line
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module CmdLine (parseCmd) where

import System.Console.GetOpt
import Control.Monad (when)

import qualified Annex
import Types

import Command
import qualified Command.Add
import qualified Command.Unannex
import qualified Command.Drop
import qualified Command.Move
import qualified Command.Get
import qualified Command.FromKey
import qualified Command.DropKey
import qualified Command.SetKey
import qualified Command.Fix
import qualified Command.Init
import qualified Command.Fsck
import qualified Command.Unlock
import qualified Command.Lock
import qualified Command.PreCommit

subCmds :: [SubCommand]
subCmds =
	[ SubCommand "add" path		Command.Add.seek
		"add files to annex"
	, SubCommand "get" path		Command.Get.seek
		"make content of annexed files available"
	, SubCommand "drop" path	Command.Drop.seek
		"indicate content of files not currently wanted"
	, SubCommand "move" path	Command.Move.seek
		"transfer content of files to/from another repository"
	, SubCommand "unlock" path	Command.Unlock.seek
		"unlock files for modification"
	, SubCommand "edit" path	Command.Unlock.seek
		"same as unlock"
	, SubCommand "lock" path	Command.Lock.seek
		"undo unlock command"
	, SubCommand "init" desc	Command.Init.seek
		"initialize git-annex with repository description"
	, SubCommand "unannex" path	Command.Unannex.seek
		"undo accidential add command"
	, SubCommand "pre-commit" path	Command.PreCommit.seek
		"run by git pre-commit hook"
	, SubCommand "fromkey" key	Command.FromKey.seek
		"adds a file using a specific key"
	, SubCommand "dropkey"	key	Command.DropKey.seek
		"drops annexed content for specified keys"
	, SubCommand "setkey" key	Command.SetKey.seek
		"sets annexed content for a key using a temp file"
	, SubCommand "fix" path		Command.Fix.seek
		"fix up symlinks to point to annexed content"
	, SubCommand "fsck" maybepath	Command.Fsck.seek
		"check for problems"
	]
	where
		path = "PATH ..."
		maybepath = "[PATH ...]"
		key = "KEY ..."
		desc = "DESCRIPTION"

-- Each dashed command-line option results in generation of an action
-- in the Annex monad that performs the necessary setting.
options :: [OptDescr (Annex ())]
options = [
	    Option ['f'] ["force"] (NoArg (storebool "force" True))
		"allow actions that may lose annexed data"
	  , Option ['q'] ["quiet"] (NoArg (storebool "quiet" True))
		"avoid verbose output"
	  , Option ['v'] ["verbose"] (NoArg (storebool "quiet" False))
		"allow verbose output"
	  , Option ['b'] ["backend"] (ReqArg (storestring "backend") "NAME")
		"specify default key-value backend to use"
	  , Option ['k'] ["key"] (ReqArg (storestring "key") "KEY")
		"specify a key to use"
	  , Option ['t'] ["to"] (ReqArg (storestring "torepository") "REPOSITORY")
		"specify to where to transfer content"
	  , Option ['f'] ["from"] (ReqArg (storestring "fromrepository") "REPOSITORY")
		"specify from where to transfer content"
	  ]
	where
		storebool n b = Annex.flagChange n $ FlagBool b
		storestring n s = Annex.flagChange n $ FlagString s

header :: String
header = "Usage: git-annex subcommand [option ..]"

{- Usage message with lists of options and subcommands. -}
usage :: String
usage = usageInfo header options ++ "\nSubcommands:\n" ++ cmddescs
	where
		cmddescs = unlines $ map (indent . showcmd) subCmds
		showcmd c =
			subcmdname c ++
			pad 11 (subcmdname c) ++
			subcmdparams c ++
			pad 13 (subcmdparams c) ++
			subcmddesc c
		indent l = "  " ++ l
		pad n s = replicate (n - length s) ' '

{- Parses command line and returns two lists of actions to be 
 - run in the Annex monad. The first actions configure it
 - according to command line options, while the second actions
 - handle subcommands. -}
parseCmd :: [String] -> AnnexState -> IO ([Annex Bool], [Annex Bool])
parseCmd argv state = do
	(flags, params) <- getopt
	when (null params) $ error usage
	case lookupCmd (head params) of
		[] -> error usage
		[subcommand] -> do
			actions <- prepSubCmd subcommand state (drop 1 params)
			let configactions = map (\flag -> do
				flag
				return True) flags
			return (configactions, actions)
		_ -> error "internal error: multiple matching subcommands"
	where
		getopt = case getOpt Permute options argv of
			(flags, params, []) -> return (flags, params)
			(_, _, errs) -> ioError (userError (concat errs ++ usage))
		lookupCmd cmd = filter (\c -> cmd  == subcmdname c) subCmds
