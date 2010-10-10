{- git-annex command line
 -
 - TODO: This is very rough and stupid; I would like to use
 - System.Console.CmdArgs.Implicit but it is not yet packaged in Debian.
 -}

module CmdLine where

import System.Console.GetOpt
import Types
import Annex

data Flag = Add FilePath | Push String | Pull String |
		Want FilePath | Get (Maybe FilePath) | Drop FilePath
	deriving Show

options :: [OptDescr Flag]
options =
	[ Option ['a'] ["add"] (ReqArg Add "FILE") "add file to annex"
	, Option ['p'] ["push"] (ReqArg Push "REPO") "push annex to repo"
	, Option ['P'] ["pull"] (ReqArg Pull "REPO") "pull annex from repo"
	, Option ['w'] ["want"] (ReqArg Want "FILE") "request file contents"
	, Option ['g'] ["get"] (OptArg Get "FILE") "transfer file contents"
	, Option ['d'] ["drop"] (ReqArg Drop "FILE") "indicate file content not needed"
	]

argvToFlags argv = do
	case getOpt Permute options argv of
		-- no options? add listed files
		([],p,[]  ) -> return $ map (\f -> Add f) p
		-- all options parsed, return flags
		(o,[],[]  ) -> return o
		-- error case
		(_,n,errs) -> ioError (userError (concat errs ++ usageInfo header options))
	where header = "Usage: git-annex [option] file"

dispatch :: Flag -> State -> IO ()
dispatch flag state = do
	case (flag) of
		Add f -> annexFile state f
		_ -> error "not implemented"
