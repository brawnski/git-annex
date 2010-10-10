{- git repository handling -}

module GitRepo where

import Directory
import System.Directory
import System.Path
import Data.String.Utils

{- Given a relative or absolute filename, calculates the name to use
 - relative to a git repository directory (which must be absolute).
 - This is the same form displayed and used by git. -}
gitRelative :: String -> String -> String
gitRelative repo file = drop (length absrepo) absfile
	where
		-- normalize both repo and file, so that repo
		-- will be substring of file
		absrepo = case (absNormPath "/" repo) of
			Just f -> f ++ "/"
			Nothing -> error $ "bad repo" ++ repo
		absfile = case (secureAbsNormPath absrepo file) of
			Just f -> f
			Nothing -> error $ file ++ " is not located inside git repository " ++ absrepo


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
	cwd <- getCurrentDirectory
	top <- seekUp cwd isRepoTop
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
