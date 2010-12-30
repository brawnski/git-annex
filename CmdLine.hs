{- git-annex command line parsing
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module CmdLine (
	parseCmd,
	Option,
	storeOptBool,
	storeOptString,
) where

import System.Console.GetOpt
import Control.Monad (when)
import Control.Monad.State (liftIO)

import qualified Annex
import Types
import Command

{- Each dashed command-line option results in generation of an action
 - in the Annex monad that performs the necessary setting.
 -}
type Option = OptDescr (Annex ())

storeOptBool :: FlagName -> Bool -> Annex ()
storeOptBool name val = Annex.flagChange name $ FlagBool val
storeOptString :: FlagName -> String -> Annex ()
storeOptString name val = Annex.flagChange name $ FlagString val

{- Parses command line, stores configure flags, and returns a 
 - list of actions to be run in the Annex monad. -}
parseCmd :: [String] -> String -> [Command] -> [Option] -> Annex [Annex Bool]
parseCmd argv header cmds options = do
	(flags, params) <- liftIO $ getopt
	when (null params) $ error usagemsg
	case lookupCmd (head params) of
		[] -> error usagemsg
		[command] -> do
			_ <- sequence flags
			prepCmd command (drop 1 params)
		_ -> error "internal error: multiple matching commands"
	where
		getopt = case getOpt Permute options argv of
			(flags, params, []) -> return (flags, params)
			(_, _, errs) -> ioError (userError (concat errs ++ usagemsg))
		lookupCmd cmd = filter (\c -> cmd  == cmdname c) cmds
		usagemsg = usage header cmds options

{- Usage message with lists of commands and options. -}
usage :: String -> [Command] -> [Option] -> String
usage header cmds options =
	usageInfo header options ++ "\nSubcommands:\n" ++ cmddescs
	where
		cmddescs = unlines $ map (indent . showcmd) cmds
		showcmd c =
			cmdname c ++
			pad (commandlen + 1) (cmdname c) ++
			cmdparams c ++
			pad (commandparamlen + 2) (cmdparams c) ++
			cmddesc c
		indent l = "  " ++ l
		pad n s = replicate (n - length s) ' '
		longest l = foldl max 0 $ map length l
		commandlen = longest $ map cmdname cmds
		commandparamlen = longest $ map cmdparams cmds
