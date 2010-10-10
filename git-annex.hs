{- git-annex main program
 - -}

import System.Environment
import GitRepo
import CmdLine
import Annex
import BackendList

main = do
	args <- getArgs
	flags <- argvToFlags args
	
	state <- startAnnex

	mapM (\f -> dispatch f state) flags
