{- git repository handling 
 -
 - This is written to be completely independant of git-annex and should be
 - suitable for other uses.
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
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
	configTrue,
	run,
	pipeRead,
	attributes,
	remotes,
	remotesAdd,
	repoRemoteName,
	inRepo,
	notInRepo,
	stagedFiles
) where

import Monad (when, unless)
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
import qualified Data.Map as Map hiding (map, split)
import Network.URI
import Maybe
import Char

import Utility

{- There are two types of repositories; those on local disk and those
 - accessed via an URL. -}
data RepoLocation = Dir FilePath | Url URI
	deriving (Show, Eq)

data Repo = Repo {
	location :: RepoLocation,
	config :: Map.Map String String,
	remotes :: [Repo],
	-- remoteName holds the name used for this repo in remotes
	remoteName :: Maybe String 
} deriving (Show, Eq)

newFrom l = 
	Repo {
		location = l,
		config = Map.empty,
		remotes = [],
		remoteName = Nothing
	}

{- Local Repo constructor. -}
repoFromPath :: FilePath -> Repo
repoFromPath dir = newFrom $ Dir dir

{- Remote Repo constructor. Throws exception on invalid url. -}
repoFromUrl :: String -> Repo
repoFromUrl url
	| startswith "file://" url = repoFromPath $ uriPath u
	| otherwise = newFrom $ Url u
		where u = fromJust $ parseURI url

{- User-visible description of a git repo. -}
repoDescribe Repo { remoteName = Just name } = name
repoDescribe Repo { location = Url url } = show url
repoDescribe Repo { location = Dir dir } = dir

{- Constructs and returns an updated version of a repo with
 - different remotes list. -}
remotesAdd :: Repo -> [Repo] -> Repo
remotesAdd repo rs = repo { remotes = rs }

{- Returns the name of the remote that corresponds to the repo, if 
 - it is a remote. Otherwise, "" -}
repoRemoteName Repo { remoteName = Just name } = name
repoRemoteName _ = ""

{- Some code needs to vary between URL and normal repos,
 - or bare and non-bare, these functions help with that. -}
repoIsUrl Repo { location = Url _ } = True
repoIsUrl _ = False

repoIsSsh Repo { location = Url url } 
	| uriScheme url == "ssh:" = True
	| otherwise = False
repoIsSsh _ = False

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
assertSsh repo action =
	if (repoIsSsh repo)
		then action
		else error $ "unsupported url in repo " ++ (repoDescribe repo)
bare :: Repo -> Bool
bare repo = case Map.lookup "core.bare" $ config repo of
	Just v -> configTrue v
	Nothing -> error $ "it is not known if git repo " ++
			(repoDescribe repo) ++
			" is a bare repository; config not read"

{- Path to a repository's gitattributes file. -}
attributes :: Repo -> String
attributes repo
	| bare repo = (workTree repo) ++ "/info/.gitattributes"
	| otherwise = (workTree repo) ++ "/.gitattributes"

{- Path to a repository's .git directory, relative to its workTree. -}
dir :: Repo -> String
dir repo
	| bare repo = ""
	| otherwise = ".git"

{- Path to a repository's --work-tree, that is, its top.
 -
 - Note that for URL repositories, this is the path on the remote host. -}
workTree :: Repo -> FilePath
workTree r@(Repo { location = Url _ }) = urlPath r
workTree (Repo { location = Dir d }) = d

{- Given a relative or absolute filename in a repository, calculates the
 - name to use to refer to the file relative to a git repository's top.
 - This is the same form displayed and used by git. -}
relative :: Repo -> String -> String
relative repo@(Repo { location = Dir d }) file = drop (length absrepo) absfile
	where
		-- normalize both repo and file, so that repo
		-- will be substring of file
		absrepo = case (absNormPath "/" d) of
			Just f -> f ++ "/"
			Nothing -> error $ "bad repo" ++ (repoDescribe repo)
		absfile = case (secureAbsNormPath absrepo file) of
			Just f -> f
			Nothing -> error $ file ++ " is not located inside git repository " ++ absrepo
relative repo file = assertLocal repo $ error "internal"

{- Hostname of an URL repo. (May include a username and/or port too.) -}
urlHost :: Repo -> String
urlHost Repo { location = Url u } = uriUserInfo a ++ uriRegName a ++ uriPort a
	where a = fromJust $ uriAuthority $ u
urlHost repo = assertUrl repo $ error "internal"

{- Path of an URL repo. -}
urlPath :: Repo -> String
urlPath Repo { location = Url u } = uriPath u
urlPath repo = assertUrl repo $ error "internal"

{- Constructs a git command line operating on the specified repo. -}
gitCommandLine :: Repo -> [String] -> [String]
gitCommandLine repo@(Repo { location = Dir d} ) params =
	-- force use of specified repo via --git-dir and --work-tree
	["--git-dir="++d++"/"++(dir repo), "--work-tree="++d] ++ params
gitCommandLine repo _ = assertLocal repo $ error "internal"

{- Runs git in the specified repo, throwing an error if it fails. -}
run :: Repo -> [String] -> IO ()
run repo params = assertLocal repo $ do
	ok <- boolSystem "git" (gitCommandLine repo params)
	unless (ok) $ error $ "git " ++ (show params) ++ " failed"

{- Runs a git subcommand and returns its output. -}
pipeRead :: Repo -> [String] -> IO String
pipeRead repo params = assertLocal repo $ do
	pOpen ReadFromPipe "git" (gitCommandLine repo params) $ \h -> do
		ret <- hGetContentsStrict h
		return ret

{- Passed a location, recursively scans for all files that
 - are checked into git at that location. -}
inRepo :: Repo -> FilePath -> IO [FilePath]
inRepo repo location = pipeNullSplit repo
	["ls-files", "--cached", "--exclude-standard", "-z", location]

{- Passed a location, recursively scans for all files that are not checked
 - into git, and not gitignored. -}
notInRepo :: Repo -> FilePath -> IO [FilePath]
notInRepo repo location = pipeNullSplit repo
	["ls-files", "--others", "--exclude-standard", "-z", location]

{- Passed a location, returns a list of the files, staged for
 - commit, that are being added, moved, or changed (but not deleted). -}
stagedFiles :: Repo -> FilePath -> IO [FilePath]
stagedFiles repo location = pipeNullSplit repo
	["diff", "--cached", "--name-only", "--diff-filter=ACMRT", "-z",
	 "HEAD", location]

{- Reads null terminated output of a git command (as enabled by the -z 
 - parameter), and splits it into a list of files. -}
pipeNullSplit :: Repo -> [String] -> IO [FilePath]
pipeNullSplit repo params = do
	fs0 <- pipeRead repo params
	return $ split0 fs0
	where
		split0 s = filter (not . null) $ split "\0" s

{- Runs git config and populates a repo with its config. -}
configRead :: Repo -> IO Repo
configRead repo@(Repo { location = Dir d }) = do
	{- Cannot use pipeRead because it relies on the config having
	   been already read. Instead, chdir to the repo. -}
	cwd <- getCurrentDirectory
	bracket_ (changeWorkingDirectory d)
		(\_ -> changeWorkingDirectory cwd) $
			pOpen ReadFromPipe "git" ["config", "--list"] $
				hConfigRead repo
configRead repo = assertSsh repo $ do
	pOpen ReadFromPipe "ssh" [urlHost repo, sshcommand] $ hConfigRead repo
	where
		sshcommand = "cd " ++ (shellEscape $ urlPath repo) ++
				" && git config --list"
hConfigRead repo h = do
	val <- hGetContentsStrict h
	let r = repo { config = configParse val }
	return r { remotes = configRemotes r }	

{- Checks if a string fron git config is a true value. -}
configTrue :: String -> Bool
configTrue s = map toLower s == "true"

{- Calculates a list of a repo's configured remotes, by parsing its config. -}
configRemotes :: Repo -> [Repo]
configRemotes repo = map construct remotes
	where
		remotes = Map.toList $ filter $ config repo
		filter = Map.filterWithKey (\k _ -> isremote k)
		isremote k = (startswith "remote." k) && (endswith ".url" k)
		remotename k = (split "." k) !! 1
		construct (k,v) = (gen v) { remoteName = Just $ remotename k }
		gen v	| isURI v = repoFromUrl v
			| otherwise = repoFromPath v

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
configMap :: Repo -> Map.Map String String
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
