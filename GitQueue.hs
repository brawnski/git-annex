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
	size,
	run
) where

import qualified Data.Map as M
import System.IO
import System.Cmd.Utils
import Data.String.Utils
import Control.Monad (unless, forM_)
import Utility

import qualified GitRepo as Git

{- An action to perform in a git repository. The file to act on
 - is not included, and must be able to be appended after the params. -}
data Action = Action {
		getSubcommand :: String,
		getParams :: [CommandParam]
	} deriving (Show, Eq, Ord)

{- A queue of actions to perform (in any order) on a git repository,
 - with lists of files to perform them on. This allows coalescing 
 - similar git commands. -}
data Queue = Queue Integer (M.Map Action [FilePath])
	deriving (Show, Eq)

{- Constructor for empty queue. -}
empty :: Queue
empty = Queue 0 M.empty

{- Adds an action to a queue. -}
add :: Queue -> String -> [CommandParam] -> FilePath -> Queue
add (Queue n m) subcommand params file = Queue (n + 1) m'
	where
		action = Action subcommand params
		m' = M.insertWith' (++) action [file] m

{- Number of items in a queue. -}
size :: Queue -> Integer
size (Queue n _) = n

{- Runs a queue on a git repository. -}
run :: Git.Repo -> Queue -> IO ()
run repo (Queue _ m) = do
	forM_ (M.toList m) $ uncurry $ runAction repo
	return ()

{- Runs an Action on a list of files in a git repository.
 -
 - Complicated by commandline length limits. -}
runAction :: Git.Repo -> Action -> [FilePath] -> IO ()
runAction repo action files = do
	unless (null files) runxargs
	where
		runxargs = pOpen WriteToPipe "xargs" ("-0":"git":params) feedxargs
		params = toCommand $ Git.gitCommandLine repo
			(Param (getSubcommand action):getParams action)
		feedxargs h = hPutStr h $ join "\0" files
