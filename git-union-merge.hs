{- git-union-merge program
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

import System.Environment

import GitUnionMerge
import qualified GitRepo as Git

header :: String
header = "Usage: git-union-merge ref ref newref"

usage :: IO a
usage = error $ "bad parameters\n\n" ++ header

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
	unionMerge g aref bref newref
