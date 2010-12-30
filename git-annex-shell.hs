{- git-annex-shell main program
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

import System.Console.GetOpt
import System.Environment
import Control.Monad (when)

import CmdLine
import Command
import Utility
import Options

import qualified Command.FromKey
import qualified Command.DropKey
import qualified Command.SetKey

cmds :: [Command]
cmds = concat
	[ Command.FromKey.command
	, Command.DropKey.command
	, Command.SetKey.command
	]

options :: [Option]
options = [ Option ['c'] ["command"] (NoArg (storeOptBool "command" True))
		"ignored for compatability with git-shell"
	  ] ++ commonOptions

header :: String
header = "Usage:\n" ++
	"\tgit-annex-shell -c git-annex command [option ..]\n" ++
	"\tgit-annex-shell -c shellcommand argument"

main :: IO ()
main = do
	args <- getArgs
	-- dispatch git-annex commands to builtin versions,
	-- and pass everything else to git-shell
	case args of
		("git-annex":as) -> builtin as
		[] -> builtin []
		_ -> external args
	where
		builtin l = dispatch l cmds options header
		external l = do
			ret <- boolSystem "git-shell" l
			when (not ret) $
				error "git-shell failed"
