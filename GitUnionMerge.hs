{- git-union-merge library
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module GitUnionMerge (
	unionMerge
) where

import System.FilePath
import System.Directory
import System.Cmd.Utils
import System.Posix.Env (setEnv, unsetEnv)
import Control.Monad (when)
import Data.List
import Data.Maybe
import Data.String.Utils

import qualified GitRepo as Git
import Utility

unionMerge :: Git.Repo -> String -> String -> String -> IO ()
unionMerge g aref bref newref = do
	setup g
	stage g aref bref
	commit g aref bref newref
	cleanup g

tmpIndex :: Git.Repo -> FilePath
tmpIndex g = Git.workTree g </> Git.gitDir g </> "index.git-union-merge"

{- Configures git to use a temporary index file. -}
setup :: Git.Repo -> IO ()
setup g = do
	cleanup g -- idempotency
	setEnv "GIT_INDEX_FILE" (tmpIndex g) True

cleanup :: Git.Repo -> IO ()
cleanup g = do
	unsetEnv "GIT_INDEX_FILE"
	e' <- doesFileExist (tmpIndex g)
	when e' $ removeFile (tmpIndex g)

{- Stages the content of both refs into the index. -}
stage :: Git.Repo -> String -> String -> IO ()
stage g aref bref = do
	-- Get the contents of aref, as a starting point.
	ls <- fromgit
		["ls-tree", "-z", "-r", "--full-tree", aref]
	-- Identify files that are different between aref and bref, and
	-- inject merged versions into git.
	diff <- fromgit
		["diff-tree", "--raw", "-z", "-r", "--no-renames", "-l0", aref, bref]
	ls' <- mapM mergefile (pairs diff)
	-- Populate the index file. Later lines override earlier ones.
	togit ["update-index", "-z", "--index-info"]
		(join "\0" $ ls++catMaybes ls')
	where
		fromgit l = Git.pipeNullSplit g (map Param l)
		togit l content = Git.pipeWrite g (map Param l) content
			>>= forceSuccess
		tofromgit l content = do
			(h, s) <- Git.pipeWriteRead g (map Param l) content
			length s `seq` do
				forceSuccess h
				Git.reap
				return ((), s)

		pairs [] = []
		pairs (_:[]) = error "parse error"
		pairs (a:b:rest) = (a,b):pairs rest
		
		nullsha = take shaSize $ repeat '0'
		ls_tree_line sha file = "100644 blob " ++ sha ++ "\t" ++ file
		unionmerge = unlines . nub . lines
		
		mergefile (info, file) = do
			let [_colonamode, _bmode, asha, bsha, _status] = words info
			if bsha == nullsha
				then return Nothing -- already staged from aref
				else mergefile' file asha bsha
		mergefile' file asha bsha = do
			let shas = filter (/= nullsha) [asha, bsha]
			content <- Git.pipeRead g $ map Param ("show":shas)
			sha <- getSha "hash-object" $
				tofromgit ["hash-object", "-w", "--stdin"] $
					unionmerge content
			return $ Just $ ls_tree_line sha file

{- Commits the index into the specified branch. -}
commit :: Git.Repo -> String -> String -> String -> IO ()
commit g aref bref newref = do
	tree <- getSha "write-tree"  $
		pipeFrom "git" ["write-tree"]
	sha <- getSha "commit-tree" $
		pipeBoth "git" ["commit-tree", tree, "-p", aref, "-p", bref]
			"union merge"
	Git.run g "update-ref" [Param newref, Param sha]

{- Runs an action that causes a git subcommand to emit a sha, and strips
   any trailing newline, returning the sha. -}
getSha :: String -> IO (a, String) -> IO String
getSha subcommand a = do
	(_, t) <- a
	let t' = if last t == '\n'
		then take (length t - 1) t
		else t
	when (length t' /= shaSize) $
		error $ "failed to read sha from git " ++ subcommand ++ " (" ++ t' ++ ")"
	return t'

shaSize :: Int
shaSize = 40
