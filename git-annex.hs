{- git-annex main program
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

import System.Environment
import System.Console.GetOpt

import qualified GitRepo as Git
import CmdLine
import Command
import Options

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
cmds = concat
	[ Command.Add.command
	, Command.Get.command
	, Command.Drop.command
	, Command.Move.command
	, Command.Copy.command
	, Command.Unlock.command
	, Command.Lock.command
	, Command.Init.command
	, Command.Unannex.command
	, Command.Uninit.command
	, Command.PreCommit.command
	, Command.Trust.command
	, Command.Untrust.command
	, Command.FromKey.command
	, Command.DropKey.command
	, Command.SetKey.command
	, Command.Fix.command
	, Command.Fsck.command
	, Command.Unused.command
	, Command.DropUnused.command
	, Command.Find.command
	]

options :: [Option]
options = commonOptions ++
	[ Option ['k'] ["key"] (ReqArg (storeOptString "key") paramKey)
		"specify a key to use"
	, Option ['t'] ["to"] (ReqArg (storeOptString "torepository") paramRemote)
		"specify to where to transfer content"
	, Option ['f'] ["from"] (ReqArg (storeOptString "fromrepository") paramRemote)
		"specify from where to transfer content"
	, Option ['x'] ["exclude"] (ReqArg (storeOptString "exclude") paramGlob)
		"skip files matching the glob pattern"
	]

header :: String
header = "Usage: git-annex command [option ..]"

main :: IO ()
main = do
	args <- getArgs
	gitrepo <- Git.repoFromCwd
	dispatch gitrepo args cmds options header
