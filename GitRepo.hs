{- git repository handling -}

module GitRepo where

import Directory
import System.Directory
import Data.String.Utils

{- Returns the path to the current repository's .git directory.
 - (For a bare repository, that is the root of the repository.) -}
gitDir :: IO String
gitDir = do
	repo <- repoTop
	bare <- isBareRepo repo
	if (bare)
		then return repo
		else return $ repo ++ "/.git"

{- Finds the top of the current git repository, which may be in a parent
 - directory. -}
repoTop :: IO String
repoTop = do
	dir <- getCurrentDirectory
	top <- seekUp dir isRepoTop
	case top of
		(Just dir) -> return dir
		Nothing -> error "Not in a git repository."

seekUp :: String -> (String -> IO Bool) -> IO (Maybe String)
seekUp dir want = do
	ok <- want dir
	if ok
		then return (Just dir)
		else case (parentDir dir) of
			(Just d) -> seekUp d want
			Nothing -> return Nothing

parentDir :: String -> Maybe String
parentDir dir =
	if length dirs > 0
	then Just ("/" ++ (join "/" $ take ((length dirs) - 1) dirs))
	else Nothing
		where
			dirs = filter (\x -> length x > 0) $ split "/" dir

isRepoTop dir = do
	r <- isGitRepo dir
	b <- isBareRepo dir
	return (r || b)

isGitRepo dir = gitSignature dir ".git" ".git/config"
isBareRepo dir = gitSignature dir "objects" "config"
	
gitSignature dir subdir file = do
	s <- (doesDirectoryExist (dir ++ "/" ++ subdir))
	f <- (doesFileExist (dir ++ "/" ++ file))
	return (s && f)
