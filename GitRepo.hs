{- git repository handling 
 -
 - This is written to be completely independant of git-annex and should be
 - suitable for other uses.
 -
 -}

module GitRepo (
	Repo,
	repoFromCwd,
	repoFromPath,
	repoFromUrl,
	repoIsLocal,
	repoIsRemote,
	repoDescribe,
	workTree,
	dir,
	relative,
	configGet,
	configMap,
	configRead,
	run,
	pipeRead,
	attributes,
	remotes,
	remotesAdd,
	repoRemoteName,
	inGit,
	notInGit
) where

import Directory
import System
import System.Directory
import System.Posix.Directory
import System.Path
import System.Cmd
import System.Cmd.Utils
import System.IO
import IO (bracket_)
import Data.String.Utils
import Data.Map as Map hiding (map, split)
import Network.URI
import Maybe
import Utility

{- A git repository can be on local disk or remote. Not to be confused
 - with a git repo's configured remotes, some of which may be on local
 - disk. -}
data Repo = 
	LocalRepo {
		top :: FilePath,
		config :: Map String String,
		remotes :: [Repo],
		-- remoteName holds the name used for this repo in remotes
		remoteName :: Maybe String 
	} | RemoteRepo {
		url :: String,
		top :: FilePath,
		config :: Map String String,
		remotes :: [Repo],
		remoteName :: Maybe String
	} deriving (Show, Read, Eq)

{- Local Repo constructor. -}
repoFromPath :: FilePath -> Repo
repoFromPath dir =
	LocalRepo {
		top = dir,
		config = Map.empty,
		remotes = [],
		remoteName = Nothing
	}

{- Remote Repo constructor. Throws exception on invalid url. -}
repoFromUrl :: String -> Repo
repoFromUrl url =
	RemoteRepo {
		url = url,
		top = path url,
		config = Map.empty,
		remotes = [],
		remoteName = Nothing
	}
	where path url = uriPath $ fromJust $ parseURI url

{- User-visible description of a git repo. -}
repoDescribe repo = 
	if (isJust $ remoteName repo)
		then fromJust $ remoteName repo
		else if (repoIsLocal repo)
			then top repo
			else url repo

{- Constructs and returns an updated version of a repo with
 - different remotes list. -}
remotesAdd :: Repo -> [Repo] -> Repo
remotesAdd repo rs = repo { remotes = rs }

{- Returns the name of the remote that corresponds to the repo, if 
 - it is a remote. Otherwise, "" -}
repoRemoteName r = 
	if (isJust $ remoteName r)
		then fromJust $ remoteName r
		else ""

{- Some code needs to vary between remote and local repos, or bare and
 - non-bare, these functions help with that. -}
repoIsLocal repo = case (repo) of
	LocalRepo {} -> True
	RemoteRepo {} -> False
repoIsRemote repo = not $ repoIsLocal repo
assertlocal repo action = 
	if (repoIsLocal repo)
		then action
		else error $ "acting on remote git repo " ++  (repoDescribe repo) ++ 
				" not supported"
bare :: Repo -> Bool
bare repo = 
	if (member b (config repo))
		then ("true" == fromJust (Map.lookup b (config repo)))
		else error $ "it is not known if git repo " ++ (repoDescribe repo) ++
			" is a bare repository; config not read"
	where
		b = "core.bare"

{- Path to a repository's gitattributes file. -}
attributes :: Repo -> String
attributes repo = assertlocal repo $ do
	if (bare repo)
		then (top repo) ++ "/info/.gitattributes"
		else (top repo) ++ "/.gitattributes"

{- Path to a repository's .git directory, relative to its topdir. -}
dir :: Repo -> String
dir repo = assertlocal repo $
	if (bare repo)
		then ""
		else ".git"

{- Path to a repository's --work-tree. -}
workTree :: Repo -> FilePath
workTree repo = top repo

{- Given a relative or absolute filename in a repository, calculates the
 - name to use to refer to the file relative to a git repository's top.
 - This is the same form displayed and used by git. -}
relative :: Repo -> String -> String
relative repo file = drop (length absrepo) absfile
	where
		-- normalize both repo and file, so that repo
		-- will be substring of file
		absrepo = case (absNormPath "/" (top repo)) of
			Just f -> f ++ "/"
			Nothing -> error $ "bad repo" ++ (top repo)
		absfile = case (secureAbsNormPath absrepo file) of
			Just f -> f
			Nothing -> error $ file ++ " is not located inside git repository " ++ absrepo

{- Constructs a git command line operating on the specified repo. -}
gitCommandLine :: Repo -> [String] -> [String]
gitCommandLine repo params = assertlocal repo $
	-- force use of specified repo via --git-dir and --work-tree
	["--git-dir="++(top repo)++"/"++(dir repo), "--work-tree="++(top repo)] ++ params

{- Runs git in the specified repo. -}
run :: Repo -> [String] -> IO ()
run repo params = assertlocal repo $ do
	r <- rawSystem "git" (gitCommandLine repo params)
	return ()

{- Runs a git subcommand and returns its output. -}
pipeRead :: Repo -> [String] -> IO String
pipeRead repo params = assertlocal repo $ do
	pOpen ReadFromPipe "git" (gitCommandLine repo params) $ \h -> do
	ret <- hGetContentsStrict h
	return ret

{- Passed a location, recursively scans for all files that
 - are checked into git at that location. -}
inGit :: Repo -> FilePath -> IO [FilePath]
inGit repo location = do
	s <- pipeRead repo ["ls-files", "--cached", "--exclude-standard"]
	return $ lines s

{- Passed a location, recursively scans for all files that are not checked
 - into git, and not gitignored. -}
notInGit :: Repo -> FilePath -> IO [FilePath]
notInGit repo location = do
	s <- pipeRead repo ["ls-files", "--others", "--exclude-standard"]
	return $ lines s

{- Runs git config and populates a repo with its config. -}
configRead :: Repo -> IO Repo
configRead repo = assertlocal repo $ do
	{- Cannot use pipeRead because it relies on the config having
           been already read. Instead, chdir to the repo. -}
	cwd <- getCurrentDirectory
	bracket_ (changeWorkingDirectory (top repo))
		(\_ -> changeWorkingDirectory cwd) $
			pOpen ReadFromPipe "git" ["config", "--list"] $ \h -> do
				val <- hGetContentsStrict h
				let r = repo { config = configParse val }
				return r { remotes = configRemotes r }

{- Calculates a list of a repo's configured remotes, by parsing its config. -}
configRemotes :: Repo -> [Repo]
configRemotes repo = map construct remotes
	where
		remotes = toList $ filter $ config repo
		filter = filterWithKey (\k _ -> isremote k)
		isremote k = (startswith "remote." k) && (endswith ".url" k)
		remotename k = (split "." k) !! 1
		construct (k,v) = (gen v) { remoteName = Just $ remotename k }
		gen v = if (isURI v)
			then repoFromUrl v
			else repoFromPath v

{- Parses git config --list output into a config map. -}
configParse :: String -> Map.Map String String
configParse s = Map.fromList $ map pair $ lines s
	where
		pair l = (key l, val l)
		key l = (keyval l) !! 0
		val l = join sep $ drop 1 $ keyval l
		keyval l = split sep l :: [String]
		sep = "="

{- Returns a single git config setting, or a default value if not set. -}
configGet :: Repo -> String -> String -> String
configGet repo key defaultValue = 
	Map.findWithDefault defaultValue key (config repo)

{- Access to raw config Map -}
configMap :: Repo -> Map String String
configMap repo = config repo

{- Finds the current git repository, which may be in a parent directory. -}
repoFromCwd :: IO Repo
repoFromCwd = do
	cwd <- getCurrentDirectory
	top <- seekUp cwd isRepoTop
	case top of
		(Just dir) -> return $ repoFromPath dir
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
	r <- isRepo dir
	b <- isBareRepo dir
	return (r || b)
	where
		isRepo dir = gitSignature dir ".git" ".git/config"
		isBareRepo dir = gitSignature dir "objects" "config"
		gitSignature dir subdir file = do
			s <- (doesDirectoryExist (dir ++ "/" ++ subdir))
			f <- (doesFileExist (dir ++ "/" ++ file))
			return (s && f)
