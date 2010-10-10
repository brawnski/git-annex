{- git repository handling -}

module GitRepo where

import Directory
import System.Directory
import System.Path
import Data.String.Utils
import Utility

{- Long-term state is stored in files inside the .git-annex directory
 - in the git repository. -}
stateLoc = ".git-annex"
gitStateDir :: String -> String
gitStateDir repo = repo ++ "/" ++ stateLoc ++ "/"

{- Path to a repository's gitattributes file. -}
gitAttributes :: FilePath -> IO String
gitAttributes repo = do
	bare <- isBareRepo repo
	if (bare)
		then return $ repo ++ "/info/.gitattributes"
		else return $ repo ++ "/.gitattributes"

{- Path to a repository's .git directory.
 - (For a bare repository, that is the root of the repository.)
 - TODO: support GIT_DIR -}
gitDir :: FilePath -> IO String
gitDir repo = do
	bare <- isBareRepo repo
	if (bare)
		then return $ repo
		else return $ repo ++ "/.git"

{- Given a relative or absolute filename, calculates the name to use
 - relative to a git repository directory (which must be absolute).
 - This is the same form displayed and used by git. -}
gitRelative :: FilePath -> String -> String
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

{- Sets up a git repo for git-annex. May be called repeatedly. -}
gitPrep :: FilePath -> IO ()
gitPrep repo = do
	-- configure git to use union merge driver on state files
	let attrLine = stateLoc ++ "/* merge=union"
	attributes <- gitAttributes repo
	exists <- doesFileExist attributes
	if (not exists)
		then writeFile attributes $ attrLine ++ "\n"
		else do
			content <- readFile attributes
			if (all (/= attrLine) (lines content))
				then do
					appendFile attributes $ attrLine ++ "\n"
					-- TODO check attributes file into git?
				else return ()

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
			"" -> return Nothing
			d -> seekUp d want

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
