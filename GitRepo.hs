{- git repository handling 
 -
 - This is written to be completely independant of git-annex and should be
 - suitable for other uses.
 -
 - -}

module GitRepo (
	GitRepo,
	gitRepoFromCwd,
	gitRepoFromPath,
	gitRepoFromUrl,
	gitWorkTree,
	gitDir,
	gitRelative,
	gitConfig,
	gitAdd,
	gitAttributes
) where

import Directory
import System
import System.Directory
import System.Path
import System.Cmd.Utils
import System.IO
import System.Posix.Process
import Data.String.Utils
import Data.Map as Map hiding (map, split)
import Network.URI
import Maybe
import Utility

{- A git repository can be on local disk or remote. Not to be confused
 - with a git repo's configured remotes, some of which may be on local
 - disk. -}
data GitRepo = 
	LocalGitRepo {
		top :: FilePath,
		bare :: Bool,
		config :: Map String String
	} | RemoteGitRepo {
		url :: String,
		top :: FilePath,
		config :: Map String String
	} deriving (Show, Read, Eq)

{- Local GitRepo constructor. -}
gitRepoFromPath :: FilePath -> IO GitRepo
gitRepoFromPath dir = do
	b <- isBareRepo dir

	let r = LocalGitRepo {
		top = dir,
		bare = b,
		config = Map.empty
	}
	r' <- gitConfigRead r

	return r'

{- Remote GitRepo constructor. Note that remote repo config is not read. 
 - Throws exception on invalid url. -}
gitRepoFromUrl :: String -> IO GitRepo
gitRepoFromUrl url = do
	return $ RemoteGitRepo {
		url = url,
		top = path url,
		config = Map.empty
	}
	where path url = uriPath $ fromJust $ parseURI url

{- Some code needs to vary between remote and local repos, these functions
 - help with that. -}
local repo = case (repo) of
	LocalGitRepo {} -> True
	RemoteGitRepo {} -> False
remote repo = not $ local repo
assertlocal repo action = 
	if (local repo)
		then action
		else error "acting on remote git repo not supported"

{- Path to a repository's gitattributes file. -}
gitAttributes :: GitRepo -> String
gitAttributes repo = assertlocal repo $ do
	if (bare repo)
		then (top repo) ++ "/info/.gitattributes"
		else (top repo) ++ "/.gitattributes"

{- Path to a repository's .git directory.
 - (For a bare repository, that is the root of the repository.)
 - TODO: support GIT_DIR -}
gitDir :: GitRepo -> String
gitDir repo = assertlocal repo $
	if (bare repo)
		then top repo
		else top repo ++ "/.git"

{- Path to a repository's --work-tree. -}
gitWorkTree :: GitRepo -> FilePath
gitWorkTree repo = top repo

{- Given a relative or absolute filename in a repository, calculates the
 - name to use to refer to the file relative to a git repository's top.
 - This is the same form displayed and used by git. -}
gitRelative :: GitRepo -> String -> String
gitRelative repo file = drop (length absrepo) absfile
	where
		-- normalize both repo and file, so that repo
		-- will be substring of file
		absrepo = case (absNormPath "/" (top repo)) of
			Just f -> f ++ "/"
			Nothing -> error $ "bad repo" ++ (top repo)
		absfile = case (secureAbsNormPath absrepo file) of
			Just f -> f
			Nothing -> error $ file ++ " is not located inside git repository " ++ absrepo

{- Stages a changed file in git's index. -}
gitAdd :: GitRepo -> FilePath -> IO ()
gitAdd repo file = runGit repo ["add", file]

{- Constructs a git command line operating on the specified repo. -}
gitCommandLine :: GitRepo -> [String] -> [String]
gitCommandLine repo params =
	-- force use of specified repo via --git-dir and --work-tree
	if (local repo) 
		then ["--git-dir="++(gitDir repo), "--work-tree="++(top repo)] ++ params
		else error "gitCommandLine not implemented for remote repo"

{- Runs git in the specified repo. -}
runGit :: GitRepo -> [String] -> IO ()
runGit repo params =
	if (local repo) 
		then do
			r <- executeFile "git" True (gitCommandLine repo params) Nothing
			return ()
		else error "runGit not implemented for remote repo"

{- Runs a git subcommand and returns its output. -}
gitPipeRead :: GitRepo -> [String] -> IO String
gitPipeRead repo params =
	if (local repo)
		then pOpen ReadFromPipe "git" (gitCommandLine repo params) $ \h -> do
			ret <- hGetContentsStrict h
			return ret
		else error "gitPipeRead not implemented for remote repo"

{- Runs git config and populates a repo with its settings. -}
gitConfigRead :: GitRepo -> IO GitRepo
gitConfigRead repo =
	if (local repo)
		then do
			c <- gitPipeRead repo ["config", "--list"]
			return repo { config = gitConfigParse c }
		else error "gitConfigRead not implemented for remote repo"

{- Parses git config --list output into a config map. -}
gitConfigParse :: String -> Map.Map String String
gitConfigParse s = Map.fromList $ map pair $ lines s
	where
		pair l = (key l, val l)
		key l = (keyval l) !! 0
		val l = join sep $ drop 1 $ keyval l
		keyval l = split sep l :: [String]
		sep = "="

{- Returns a single git config setting, or a default value if not set. -}
gitConfig :: GitRepo -> String -> String -> String
gitConfig repo key defaultValue = 
	Map.findWithDefault key defaultValue (config repo)

{- Returns a list of a repo's configured remotes. -}
gitConfigRemotes :: GitRepo -> IO [GitRepo]
gitConfigRemotes repo = mapM construct remotes
	where
		remotes = elems $ filter $ config repo
		filter = filterWithKey (\k _ -> isremote k)
		isremote k = (startswith "remote." k) && (endswith ".url" k)
		construct r =
			if (isURI r)
				then gitRepoFromUrl r
				else gitRepoFromPath r

{- Finds the current git repository, which may be in a parent directory. -}
gitRepoFromCwd :: IO GitRepo
gitRepoFromCwd = do
	cwd <- getCurrentDirectory
	top <- seekUp cwd isRepoTop
	case top of
		(Just dir) -> gitRepoFromPath dir
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
