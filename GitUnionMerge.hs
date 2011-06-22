{- git-union-merge library
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module GitUnionMerge (
	merge,
	commit
) where

import System.Cmd.Utils
import Data.List
import Data.Maybe
import Data.String.Utils

import qualified GitRepo as Git
import Utility

{- Performs a union merge between two branches, staging it in the index.
 - Any previously staged changes in the index will be lost.
 -
 - When only one branch is specified, it is merged into the index.
 - In this case, previously staged changes in the index are preserved.
 -
 - Should be run with a temporary index file configured by Git.useIndex.
 -}
merge :: Git.Repo -> [String] -> IO ()
merge g (x:y:[]) = do
	a <- ls_tree g x
	b <- merge_trees g x y
	update_index g (a++b)
merge g [x] = merge_tree_index g x >>= update_index g
merge _ _ = error "wrong number of branches to merge"

{- Feeds a list into update-index. Later items in the list can override
 - earlier ones, so the list can be generated from any combination of
 - ls_tree, merge_trees, and merge_tree_index. -}
update_index :: Git.Repo -> [String] -> IO ()
update_index g l =  togit ["update-index", "-z", "--index-info"] (join "\0" l)
	where
		togit ps content = Git.pipeWrite g (map Param ps) content
			>>= forceSuccess

{- Gets the contents of a tree in a format suitable for update_index. -}
ls_tree :: Git.Repo -> String -> IO [String]
ls_tree g x = Git.pipeNullSplit g $ 
	map Param ["ls-tree", "-z", "-r", "--full-tree", x]

{- For merging two trees. -}
merge_trees :: Git.Repo -> String -> String -> IO [String]
merge_trees g x y = calc_merge g
	["diff-tree", "--raw", "-z", "-r", "--no-renames", "-l0", x, y]

{- For merging a single tree into the index. -}
merge_tree_index :: Git.Repo -> String -> IO [String]
merge_tree_index g x = calc_merge g
	["diff-index", "--raw", "-z", "-r", "--no-renames", "-l0", x]

{- Calculates how to perform a merge, using git to get a raw diff,
 - and returning a list suitable for update_index. -}
calc_merge :: Git.Repo -> [String] -> IO [String]
calc_merge g differ = do
	diff <- Git.pipeNullSplit g $ map Param differ
	l <- mapM (mergeFile g) (pairs diff)
	return $ catMaybes l
	where
		pairs [] = []
		pairs (_:[]) = error "calc_merge parse error"
		pairs (a:b:rest) = (a,b):pairs rest

{- Given an info line from a git raw diff, and the filename, generates
 - a line suitable for update_index that union merges the two sides of the
 - diff. -}
mergeFile :: Git.Repo -> (String, FilePath) -> IO (Maybe String)
mergeFile g (info, file) = case filter (/= nullsha) [asha, bsha] of
	[] -> return Nothing
	(sha:[]) -> return $ Just $ ls_tree_line sha
	shas -> do
		content <- Git.pipeRead g $ map Param ("show":shas)
		sha <- Git.hashObject g $ unionmerge content
		return $ Just $ ls_tree_line sha
	where
		[_colonamode, _bmode, asha, bsha, _status] = words info
		ls_tree_line sha = "100644 blob " ++ sha ++ "\t" ++ file
		nullsha = take Git.shaSize $ repeat '0'
		unionmerge = unlines . nub . lines

{- Commits the index into the specified branch, 
 - with the specified parent refs. -}
commit :: Git.Repo -> String -> String -> [String] -> IO ()
commit g message newref parentrefs = do
	tree <- Git.getSha "write-tree" $ ignorehandle $
		pipeFrom "git" ["write-tree"]
	sha <- Git.getSha "commit-tree" $ ignorehandle $ 
		pipeBoth "git" (["commit-tree", tree] ++ ps) message
	Git.run g "update-ref" [Param newref, Param sha]
	where
		ignorehandle a = return . snd =<< a
		ps = concatMap (\r -> ["-p", r]) parentrefs
