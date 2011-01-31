{- git repository command queue
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module GitQueue (
	Queue,
	empty,
	add,
	run
) where

import qualified Data.Map as M
import System.IO
import System.Cmd.Utils
import Data.String.Utils
import Control.Monad (unless)

import qualified GitRepo as Git

{- An action to perform in a git repository. The file to act on
 - is not included, and must be able to be appended after the params. -}
data Action = Action {
		getSubcommand :: String,
		getParams :: [String]
	} deriving (Show, Eq, Ord)

{- A queue of actions to perform (in any order) on a git repository,
 - with lists of files to perform them on. This allows coalescing 
 - similar git commands. -}
type Queue = M.Map Action [FilePath]

{- Constructor for empty queue. -}
empty :: Queue
empty = M.empty

{- Adds an action to a queue. -}
add :: Queue -> String -> [String] -> FilePath -> Queue
add queue subcommand params file = M.insertWith (++) action [file] queue
	where
		action = Action subcommand params

{- Runs a queue on a git repository. -}
run :: Git.Repo -> Queue -> IO ()
run repo queue = do
	mapM_ (uncurry $ runAction repo) $ M.toList queue
	return ()

{- Runs an Action on a list of files in a git repository.
 -
 - Complicated by commandline length limits. -}
runAction :: Git.Repo -> Action -> [FilePath] -> IO ()
runAction repo action files = do
	unless (null files) runxargs
	where
		runxargs = pOpen WriteToPipe "xargs" ("-0":gitcmd) feedxargs
		gitcmd = "git" : Git.gitCommandLine repo
			(getSubcommand action:getParams action)
		feedxargs h = hPutStr h $ join "\0" files
