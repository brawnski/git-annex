{- git-annex command line
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module CmdLine (parseCmd) where

import System.Console.GetOpt
import Control.Monad (when)
import Control.Monad.State (liftIO)

import qualified Annex
import Types

import Command
import qualified Command.Add
import qualified Command.Unannex
import qualified Command.Drop
import qualified Command.Move
import qualified Command.Copy
import qualified Command.Get
import qualified Command.FromKey
import qualified Command.DropKey
import qualified Command.SetKey
import qualified Command.Fix
import qualified Command.Init
import qualified Command.Fsck
import qualified Command.Unused
import qualified Command.DropUnused
import qualified Command.Unlock
import qualified Command.Lock
import qualified Command.PreCommit
import qualified Command.Find
import qualified Command.Uninit
import qualified Command.Trust
import qualified Command.Untrust

cmds :: [Command]
cmds =
	[ Command.Add.command
	, Command "get" path		Command.Get.seek
		"make content of annexed files available"
	, Command "drop" path	Command.Drop.seek
		"indicate content of files not currently wanted"
	, Command "move" path	Command.Move.seek
		"move content of files to/from another repository"
	, Command "copy" path	Command.Copy.seek
		"copy content of files to/from another repository"
	, Command "unlock" path	Command.Unlock.seek
		"unlock files for modification"
	, Command "edit" path	Command.Unlock.seek
		"same as unlock"
	, Command "lock" path	Command.Lock.seek
		"undo unlock command"
	, Command "init" desc	Command.Init.seek
		"initialize git-annex with repository description"
	, Command "unannex" path	Command.Unannex.seek
		"undo accidential add command"
	, Command "uninit" path	Command.Uninit.seek
		"de-initialize git-annex and clean out repository"
	, Command "pre-commit" path	Command.PreCommit.seek
		"run by git pre-commit hook"
	, Command "trust" remote	Command.Trust.seek
		"trust a repository"
	, Command "untrust" remote	Command.Untrust.seek
		"do not trust a repository"
	, Command "fromkey" key	Command.FromKey.seek
		"adds a file using a specific key"
	, Command "dropkey"	key	Command.DropKey.seek
		"drops annexed content for specified keys"
	, Command "setkey" key	Command.SetKey.seek
		"sets annexed content for a key using a temp file"
	, Command "fix" path		Command.Fix.seek
		"fix up symlinks to point to annexed content"
	, Command "fsck" maybepath	Command.Fsck.seek
		"check for problems"
	, Command "unused" nothing	Command.Unused.seek
		"look for unused file content"
	, Command "dropunused" number Command.DropUnused.seek
		"drop unused file content"
	, Command "find" maybepath	Command.Find.seek
		"lists available files"
	]
	where
		path = "PATH ..."
		maybepath = "[PATH ...]"
		key = "KEY ..."
		desc = "DESCRIPTION"
		number = "NUMBER ..."
		remote = "REMOTE ..."
		nothing = ""

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
	  , Option ['x'] ["exclude"] (ReqArg (storestring "exclude") "GLOB")
		"skip files matching the glob pattern"
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
		cmddescs = unlines $ map (indent . showcmd) cmds
		showcmd c =
			cmdname c ++
			pad 11 (cmdname c) ++
			cmdparams c ++
			pad 13 (cmdparams c) ++
			cmddesc c
		indent l = "  " ++ l
		pad n s = replicate (n - length s) ' '

{- Parses command line, stores configure flags, and returns a 
 - list of actions to be run in the Annex monad. -}
parseCmd :: [String] -> Annex [Annex Bool]
parseCmd argv = do
	(flags, params) <- liftIO $ getopt
	when (null params) $ error usage
	case lookupCmd (head params) of
		[] -> error usage
		[command] -> do
			_ <- sequence flags
			prepCmd command (drop 1 params)
		_ -> error "internal error: multiple matching commands"
	where
		getopt = case getOpt Permute options argv of
			(flags, params, []) -> return (flags, params)
			(_, _, errs) -> ioError (userError (concat errs ++ usage))
		lookupCmd cmd = filter (\c -> cmd  == cmdname c) cmds
