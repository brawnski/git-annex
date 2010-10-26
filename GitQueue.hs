{- git repository command queues
 -}

module GitQueue (
	Queue,
	empty,
	add,
	run
) where

import qualified Data.Map as M

import qualified GitRepo as Git

{- An action to perform in a git repository. The file to act on
 - is not included, and must be able to be appended after the params. -}
data Action = Action {
		subcommand :: String,
		params :: [String]
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
	mapM (\(k, v) -> runAction repo k v) $ M.toList queue
	return ()

{- Runs an Action on a list of files in a git repository.
 -
 - Complicated by commandline length limits. -}
runAction :: Git.Repo -> Action -> [FilePath] -> IO ()
runAction repo action files = do
	xargs [] 0 files
	where
		arg_max = 2048 -- TODO get better ARG_MAX
		maxlen = arg_max - cmdlen
		c = (subcommand action):(params action)
		cmdlen = (length "git") + 
			(foldl (\a b -> a + b + 1) 1 $ map length c)
		xargs collect _ [] = exec collect
		xargs collect len (f:fs) = do
			let len' = len + 1 + length f
			if (len' >= maxlen)
				then do
					exec collect
					xargs [f] (length f) fs
				else xargs (f:collect) len' fs
		exec [] = return ()
		exec fs = Git.run repo $ c ++ fs
