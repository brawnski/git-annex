{- git-annex repository access with ssh
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Ssh where

import qualified Annex
import qualified GitRepo as Git
import Utility
import Types

{- Generates parameters to ssh to a repository's host and run a command.
 - Caller is responsible for doing any neccessary shellEscaping of the
 - passed command. -}
sshToRepo :: Git.Repo -> [CommandParam] -> Annex [CommandParam]
sshToRepo repo sshcmd = do
	s <- Annex.repoConfig repo "ssh-options" ""
	let sshoptions = map Param (words s)
	let sshport = case Git.urlPort repo of
		Nothing -> []
		Just p -> [Param "-p", Param (show p)]
	let sshhost = Param $ Git.urlHostUser repo
	return $ sshoptions ++ sshport ++ [sshhost] ++ sshcmd
