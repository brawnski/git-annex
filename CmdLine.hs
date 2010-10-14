{- git-annex command line
 -
 - TODO: This is very rough and stupid; I would like to use
 - System.Console.CmdArgs.Implicit but it is not yet packaged in Debian.
 -}

module CmdLine (
	argvToMode,
	dispatch,
	Mode
) where

import System.Console.GetOpt
import Types
import Annex

data Mode = Add | Push | Pull | Want | Get | Drop | Unannex
	deriving Show

options :: [OptDescr Mode]
options =
	[ Option ['a'] ["add"] (NoArg Add) "add files to annex"
	, Option ['p'] ["push"] (NoArg Push) "push annex to repos"
	, Option ['P'] ["pull"] (NoArg Pull) "pull annex from repos"
	, Option ['w'] ["want"] (NoArg Want) "request file contents"
	, Option ['g'] ["get"] (NoArg Get) "transfer file contents"
	, Option ['d'] ["drop"] (NoArg Drop) "indicate file contents not needed"
	, Option ['u'] ["unannex"] (NoArg Unannex) "undo --add"
	]

argvToMode argv = do
	case getOpt Permute options argv of
		-- default mode is Add
		([],files,[]) -> return (Add, files)
		-- one mode is normal case
		(m:[],files,[]) -> return (m, files)
		-- multiple modes is an error
		(ms,files,[]) -> ioError (userError ("only one mode should be specified\n" ++ usageInfo header options))
		-- error case
		(_,files,errs) -> ioError (userError (concat errs ++ usageInfo header options))
	where header = "Usage: git-annex [mode] file"

dispatch :: Mode -> FilePath -> Annex ()
dispatch mode item = do
	case (mode) of
		Add     -> annexFile item
		Push    -> annexPushRepo item
		Pull    -> annexPullRepo item
		Want    -> annexWantFile item
		Get     -> annexGetFile item
		Drop    -> annexDropFile item
		Unannex -> unannexFile item
