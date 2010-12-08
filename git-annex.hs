{- git-annex main program
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

import System.Environment

import qualified Annex
import Core
import Upgrade
import CmdLine
import qualified GitRepo as Git
import BackendList

main :: IO ()
main = do
	args <- getArgs
	gitrepo <- Git.repoFromCwd
	state <- Annex.new gitrepo allBackends
	(actions, state') <- Annex.run state $ parseCmd args
	tryRun state' $ [startup, upgrade] ++ actions
