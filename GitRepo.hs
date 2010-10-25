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
	repoIsUrl,
	repoIsSsh,
	repoDescribe,
	workTree,
	dir,
	relative,
	urlPath,
	urlHost,
	configGet,
	configMap,
	configRead,
	run,
	pipeRead,
	attributes,
	remotes,
	remotesAdd,
	repoRemoteName,
	inRepo,
	notInRepo
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

{- There are two types of repositories; those on local disk and those
 - accessed via an URL. -}
data Repo = 
	Repo {
		top :: FilePath,
		config :: Map String String,
		remotes :: [Repo],
		-- remoteName holds the name used for this repo in remotes
		remoteName :: Maybe String 
	} | UrlRepo {
		url :: URI,
		config :: Map String String,
		remotes :: [Repo],
		remoteName :: Maybe String
	} deriving (Show, Eq)

{- Local Repo constructor. -}
repoFromPath :: FilePath -> Repo
repoFromPath dir =
	Repo {
		top = dir,
		config = Map.empty,
		remotes = [],
		remoteName = Nothing
	}

{- Remote Repo constructor. Throws exception on invalid url. -}
repoFromUrl :: String -> Repo
repoFromUrl url =
	UrlRepo {
		url = fromJust $ parseURI url,
		config = Map.empty,
		remotes = [],
		remoteName = Nothing
	}

{- User-visible description of a git repo. -}
repoDescribe repo = 
	if (isJust $ remoteName repo)
		then fromJust $ remoteName repo
		else if (not $ repoIsUrl repo)
			then top repo
			else show (url repo)

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

{- Some code needs to vary between URL and normal repos,
 - or bare and non-bare, these functions help with that. -}
repoIsUrl repo = case (repo) of
	UrlRepo {} -> True
	Repo {} -> False
repoIsSsh repo = repoIsUrl repo && (uriScheme $ url repo) == "ssh:"
assertLocal repo action = 
	if (not $ repoIsUrl repo)
		then action
		else error $ "acting on URL git repo " ++  (repoDescribe repo) ++ 
				" not supported"
assertUrl repo action = 
	if (repoIsUrl repo)
		then action
		else error $ "acting on local git repo " ++  (repoDescribe repo) ++ 
				" not supported"
assertssh repo action =
	if (repoIsSsh repo)
		then action
		else error $ "unsupported url " ++ (show $ url repo)
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
attributes repo = assertLocal repo $ do
	if (bare repo)
		then (top repo) ++ "/info/.gitattributes"
		else (top repo) ++ "/.gitattributes"

{- Path to a repository's .git directory, relative to its workTree. -}
dir :: Repo -> String
dir repo = if (bare repo) then "" else ".git"

{- Path to a repository's --work-tree, that is, its top.
 -
 - Note that for URL repositories, this is relative to the urlHost -}
workTree :: Repo -> FilePath
workTree repo =
	if (not $ repoIsUrl repo)
		then top repo
		else urlPath repo

{- Given a relative or absolute filename in a repository, calculates the
 - name to use to refer to the file relative to a git repository's top.
 - This is the same form displayed and used by git. -}
relative :: Repo -> String -> String
relative repo file = assertLocal repo $ drop (length absrepo) absfile
	where
		-- normalize both repo and file, so that repo
		-- will be substring of file
		absrepo = case (absNormPath "/" (top repo)) of
			Just f -> f ++ "/"
			Nothing -> error $ "bad repo" ++ (top repo)
		absfile = case (secureAbsNormPath absrepo file) of
			Just f -> f
			Nothing -> error $ file ++ " is not located inside git repository " ++ absrepo

{- Hostname of an URL repo. (May include a username and/or port too.) -}
urlHost :: Repo -> String
urlHost repo = assertUrl repo $ 
	(uriUserInfo a) ++ (uriRegName a) ++ (uriPort a)
	where 
		a = fromJust $ uriAuthority $ url repo

{- Path of an URL repo. -}
urlPath :: Repo -> String
urlPath repo = assertUrl repo $
	uriPath $ url repo

{- Constructs a git command line operating on the specified repo. -}
gitCommandLine :: Repo -> [String] -> [String]
gitCommandLine repo params = assertLocal repo $
	-- force use of specified repo via --git-dir and --work-tree
	["--git-dir="++(top repo)++"/"++(dir repo), "--work-tree="++(top repo)] ++ params

{- Runs git in the specified repo. -}
run :: Repo -> [String] -> IO ()
run repo params = assertLocal repo $ do
	r <- safeSystem "git" (gitCommandLine repo params)
	return ()

{- Runs a git subcommand and returns its output. -}
pipeRead :: Repo -> [String] -> IO String
pipeRead repo params = assertLocal repo $ do
	pOpen ReadFromPipe "git" (gitCommandLine repo params) $ \h -> do
	ret <- hGetContentsStrict h
	return ret

{- Passed a location, recursively scans for all files that
 - are checked into git at that location. -}
inRepo :: Repo -> FilePath -> IO [FilePath]
inRepo repo location = do
	s <- pipeRead repo ["ls-files", "--cached", "--exclude-standard", location]
	return $ lines s

{- Passed a location, recursively scans for all files that are not checked
 - into git, and not gitignored. -}
notInRepo :: Repo -> FilePath -> IO [FilePath]
notInRepo repo location = do
	s <- pipeRead repo ["ls-files", "--others", "--exclude-standard", location]
	return $ lines s

{- Runs git config and populates a repo with its config. -}
configRead :: Repo -> IO Repo
configRead repo = 
	if (not $ repoIsUrl repo)
	then do 
		{- Cannot use pipeRead because it relies on the config having
	           been already read. Instead, chdir to the repo. -}
		cwd <- getCurrentDirectory
		bracket_ (changeWorkingDirectory (top repo))
			(\_ -> changeWorkingDirectory cwd) $
				pOpen ReadFromPipe "git" ["config", "--list"] proc
	else assertssh repo $ do
		pOpen ReadFromPipe "ssh" [urlHost repo, sshcommand] proc
	where
		sshcommand = "cd " ++ (shellEscape $ urlPath repo) ++ " && git config --list"
		proc h = do
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
