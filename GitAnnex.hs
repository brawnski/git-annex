{- git-annex main program
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module GitAnnex where

import System.Console.GetOpt

import qualified GitRepo as Git
import CmdLine
import Command
import Options
import qualified Annex

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
import qualified Command.Describe
import qualified Command.Fsck
import qualified Command.Unused
import qualified Command.DropUnused
import qualified Command.Unlock
import qualified Command.Lock
import qualified Command.PreCommit
import qualified Command.Find
import qualified Command.Whereis
import qualified Command.Migrate
import qualified Command.Uninit
import qualified Command.Trust
import qualified Command.Untrust
import qualified Command.Semitrust
import qualified Command.Map
import qualified Command.Upgrade
import qualified Command.Version

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
	, Command.Describe.command
	, Command.Unannex.command
	, Command.Uninit.command
	, Command.PreCommit.command
	, Command.Trust.command
	, Command.Untrust.command
	, Command.Semitrust.command
	, Command.FromKey.command
	, Command.DropKey.command
	, Command.SetKey.command
	, Command.Fix.command
	, Command.Fsck.command
	, Command.Unused.command
	, Command.DropUnused.command
	, Command.Find.command
	, Command.Whereis.command
	, Command.Migrate.command
	, Command.Map.command
	, Command.Upgrade.command
	, Command.Version.command
	]

options :: [Option]
options = commonOptions ++
	[ Option ['k'] ["key"] (ReqArg setkey paramKey)
		"specify a key to use"
	, Option ['t'] ["to"] (ReqArg setto paramRemote)
		"specify to where to transfer content"
	, Option ['f'] ["from"] (ReqArg setfrom paramRemote)
		"specify from where to transfer content"
	, Option ['x'] ["exclude"] (ReqArg addexclude paramGlob)
		"skip files matching the glob pattern"
	]
	where
		setkey v = Annex.changeState $ \s -> s { Annex.defaultkey = Just v }
		setto v = Annex.changeState $ \s -> s { Annex.toremote = Just v }
		setfrom v = Annex.changeState $ \s -> s { Annex.fromremote = Just v }
		addexclude v = Annex.changeState $ \s -> s { Annex.exclude = v:(Annex.exclude s) }

header :: String
header = "Usage: git-annex command [option ..]"

run :: [String] -> IO ()
run args = do
	gitrepo <- Git.repoFromCwd
	dispatch gitrepo args cmds options header
