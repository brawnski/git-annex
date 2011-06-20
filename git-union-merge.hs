{- git-union-merge program
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

import System.Environment
import System.FilePath
import System.Directory
import System.Cmd
import System.Cmd.Utils
import System.Posix.Env (setEnv)
import System.Posix.Directory (changeWorkingDirectory)
import Control.Monad (when, unless)
import Data.List

import qualified GitRepo as Git
import Utility

header :: String
header = "Usage: git-union-merge branch ref ref"

usage :: IO a
usage = error $ "bad parameters\n\n" ++ header

main :: IO ()
main = do
	[branch, aref, bref] <- parseArgs
	g <- setup
	stage g aref bref
	commit g branch aref bref
	cleanup g

parseArgs :: IO [String]
parseArgs = do
	args <- getArgs
	if (length args /= 3)
		then usage
		else return args

tmpDir :: Git.Repo -> FilePath
tmpDir g = Git.workTree g </> Git.gitDir g </> "tmp" </> "git-union-merge"

tmpIndex :: Git.Repo -> FilePath
tmpIndex g = Git.workTree g </> Git.gitDir g </> "tmp" </> "git-union-merge.index"

{- Moves to a temporary directory, and configures git to use it as its
 - working tree, and to use a temporary index file as well. -}
setup :: IO Git.Repo
setup = do
	g <- Git.configRead =<< Git.repoFromCwd
	cleanup g -- idempotency
	let tmp = tmpDir g
	createDirectoryIfMissing True tmp
	changeWorkingDirectory tmp
	-- Note that due to these variables being set, Git.run and
	-- similar helpers cannot be used, as they override the work tree.
	-- It is only safe to use Git.run etc when doing things that do
	-- not operate on the work tree.
	setEnv "GIT_WORK_TREE" tmp True
	setEnv "GIT_INDEX_FILE" (tmpIndex g) True
	return g

cleanup :: Git.Repo -> IO ()
cleanup g = do
	e <- doesDirectoryExist (tmpDir g)
	when e $ removeDirectoryRecursive (tmpDir g)
	e' <- doesFileExist (tmpIndex g)
	when e' $ removeFile (tmpIndex g)

{- Stages the content of both refs into the index. -}
stage :: Git.Repo -> String -> String -> IO ()
stage g aref bref = do
	-- populate index with the contents of aref, as a starting point
	_ <- system $ "git ls-tree -r --full-name --full-tree " ++ aref ++ 
		" | git update-index --index-info"
	-- identify files that are different in bref, and stage merged files
	diff <- Git.pipeNullSplit g $ map Param
		["diff-tree", "--raw", "-z", "--no-renames", "-l0", aref, bref]
	mapM_ genfile (pairs diff)
	_ <- system "git add ."
	return ()
	where
		pairs [] = []
		pairs (_:[]) = error "parse error"
		pairs (a:b:rest) = (a,b):pairs rest

		nullsha = take 40 $ repeat '0'

		genfile (info, file) = do
			let [_colonamode, _bmode, asha, bsha, _status] = words info
			let shas = 
				if bsha == nullsha
					then [] -- staged from aref
					else
						if asha == nullsha
							then [bsha]
							else [asha, bsha]
			unless (null shas) $ do
				content <- Git.pipeRead g $ map Param ("show":shas)
				writeFile file $ unlines $ nub $ lines content

{- Commits the index into the specified branch. -}
commit :: Git.Repo -> String -> String -> String -> IO ()
commit g branch aref bref = do
	tree <- getsha $
		pipeFrom "git" ["write-tree"]
	sha <- getsha $ 
		pipeBoth "git" ["commit-tree", tree, "-p", aref, "-p", bref]
			"union merge"
	Git.run g "update-ref" [Param $ "refs/heads/" ++ branch, Param sha]
	where
		getsha a = do
			(_, t) <- a
			let t' = if last t == '\n'
				then take (length t - 1) t
				else t
			when (null t') $ error "failed to read sha from git"
			return t'
