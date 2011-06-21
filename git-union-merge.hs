{- git-union-merge program
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

import System.Environment
import System.FilePath
import System.Directory
import Control.Monad (when)

import GitUnionMerge
import qualified GitRepo as Git

header :: String
header = "Usage: git-union-merge ref ref newref"

usage :: IO a
usage = error $ "bad parameters\n\n" ++ header

tmpIndex :: Git.Repo -> FilePath
tmpIndex g = Git.workTree g </> Git.gitDir g </> "index.git-union-merge"

setup :: Git.Repo -> IO ()
setup g = do
	cleanup g -- idempotency

cleanup :: Git.Repo -> IO ()
cleanup g = do
	e' <- doesFileExist (tmpIndex g)
	when e' $ removeFile (tmpIndex g)

parseArgs :: IO [String]
parseArgs = do
	args <- getArgs
	if (length args /= 3)
		then usage
		else return args

main :: IO ()
main = do
	[aref, bref, newref] <- parseArgs
	g <- Git.configRead =<< Git.repoFromCwd
	Git.withIndex (tmpIndex g) $ do
		setup g
		unionMerge g aref bref newref
		cleanup g
