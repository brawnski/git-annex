{- git-annex main program -}

import System.Environment
import qualified Annex
import Core
import CmdLine

main = do
	args <- getArgs
	(mode, params) <- argvToMode args
	state <- start
	tryRun state mode params
