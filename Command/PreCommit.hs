{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.PreCommit where

import Command
import qualified Command.Fix

{- Run by git pre-commit hook. -}
start :: SubCmdStartString
start file = do
	-- If a file is unlocked for edit, inject its content into the
	-- annex, and replace it with a symlink to the content. Git will
	-- then commit that.
	error "TODO"

	-- fix symlinks
	Command.Fix.start file
