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
	
	repo <- currentRepo
	gitPrep repo

	mapM (\f -> dispatch f supportedBackends repo) flags
