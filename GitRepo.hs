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
	gitDir,
	relative,
	urlPath,
	urlHost,
	configGet,
	configMap,
	configRead,
	hConfigRead,
	configStore,
	configTrue,
	gitCommandLine,
	run,
	pipeRead,
	hPipeRead,
	attributes,
	remotes,
	remotesAdd,
	repoRemoteName,
	inRepo,
	notInRepo,
	stagedFiles,
	checkAttr,
	decodeGitFile,
	encodeGitFile,
	typeChangedFiles,
	typeChangedStagedFiles,

	prop_idempotent_deencode
) where

import Control.Monad (unless)
import System.Directory
import System.Posix.Directory
import System.Path
import System.Cmd.Utils
import IO (bracket_)
import Data.String.Utils
import System.IO
import qualified Data.Map as Map hiding (map, split)
import Network.URI
import Data.Maybe
import Data.Char
import Data.Word (Word8)
import Codec.Binary.UTF8.String (encode)
import Text.Printf
import Data.List (isInfixOf)

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

newFrom :: RepoLocation -> Repo
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
repoDescribe :: Repo -> String
repoDescribe Repo { remoteName = Just name } = name
repoDescribe Repo { location = Url url } = show url
repoDescribe Repo { location = Dir dir } = dir

{- Constructs and returns an updated version of a repo with
 - different remotes list. -}
remotesAdd :: Repo -> [Repo] -> Repo
remotesAdd repo rs = repo { remotes = rs }

{- Returns the name of the remote that corresponds to the repo, if 
 - it is a remote. Otherwise, "" -}
repoRemoteName :: Repo -> String
repoRemoteName Repo { remoteName = Just name } = name
repoRemoteName _ = ""

{- Some code needs to vary between URL and normal repos,
 - or bare and non-bare, these functions help with that. -}
repoIsUrl :: Repo -> Bool
repoIsUrl Repo { location = Url _ } = True
repoIsUrl _ = False

repoIsSsh :: Repo -> Bool
repoIsSsh Repo { location = Url url } 
	| uriScheme url == "ssh:" = True
	-- git treats these the same as ssh
	| uriScheme url == "git+ssh:" = True
	| uriScheme url == "ssh+git:" = True
	| otherwise = False
repoIsSsh _ = False

assertLocal :: Repo -> a -> a
assertLocal repo action = 
	if not $ repoIsUrl repo
		then action
		else error $ "acting on URL git repo " ++  repoDescribe repo ++ 
				" not supported"
assertUrl :: Repo -> a -> a
assertUrl repo action = 
	if repoIsUrl repo
		then action
		else error $ "acting on local git repo " ++  repoDescribe repo ++ 
				" not supported"

bare :: Repo -> Bool
bare repo = case Map.lookup "core.bare" $ config repo of
	Just v -> configTrue v
	Nothing -> error $ "it is not known if git repo " ++
			repoDescribe repo ++
			" is a bare repository; config not read"

{- Path to a repository's gitattributes file. -}
attributes :: Repo -> String
attributes repo
	| bare repo = workTree repo ++ "/info/.gitattributes"
	| otherwise = workTree repo ++ "/.gitattributes"

{- Path to a repository's .git directory, relative to its workTree. -}
gitDir :: Repo -> String
gitDir repo
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
relative :: Repo -> FilePath -> IO FilePath
relative repo@(Repo { location = Dir d }) file = do
	cwd <- getCurrentDirectory
	return $ drop (length absrepo) (absfile cwd)
	where
		-- normalize both repo and file, so that repo
		-- will be substring of file
		absrepo = case (absNormPath "/" d) of
			Just f -> f ++ "/"
			Nothing -> error $ "bad repo" ++ repoDescribe repo
		absfile c = case (secureAbsNormPath c file) of
			Just f -> f
			Nothing -> error $ file ++ " is not located inside git repository " ++ absrepo
relative repo _ = assertLocal repo $ error "internal"

{- Hostname of an URL repo. (May include a username and/or port too.) -}
urlHost :: Repo -> String
urlHost Repo { location = Url u } = uriUserInfo a ++ uriRegName a ++ uriPort a
	where a = fromJust $ uriAuthority u
urlHost repo = assertUrl repo $ error "internal"

{- Path of an URL repo. -}
urlPath :: Repo -> String
urlPath Repo { location = Url u } = uriPath u
urlPath repo = assertUrl repo $ error "internal"

{- Constructs a git command line operating on the specified repo. -}
gitCommandLine :: Repo -> [String] -> [String]
gitCommandLine repo@(Repo { location = Dir d} ) params =
	-- force use of specified repo via --git-dir and --work-tree
	["--git-dir=" ++ d ++ "/" ++ gitDir repo, "--work-tree=" ++ d] ++ params
gitCommandLine repo _ = assertLocal repo $ error "internal"

{- Runs git in the specified repo, throwing an error if it fails. -}
run :: Repo -> [String] -> IO ()
run repo params = assertLocal repo $ do
	ok <- boolSystem "git" (gitCommandLine repo params)
	unless ok $ error $ "git " ++ show params ++ " failed"

{- Runs a git subcommand and returns its output. -}
pipeRead :: Repo -> [String] -> IO String
pipeRead repo params = assertLocal repo $ do
	pOpen ReadFromPipe "git" (gitCommandLine repo params) $ \h -> do
		hGetContentsStrict h

{- Like pipeRead, but does not read output strictly; recommended
 - for git commands that produce a lot of output that will be processed
 - lazily. 
 -
 - ONLY AFTER the string has been read completely, You must call either
 - getProcessStatus or forceSuccess on the PipeHandle. Zombies will result
 - otherwise.-}
hPipeRead :: Repo -> [String] -> IO (PipeHandle, String)
hPipeRead repo params = assertLocal repo $ do
	pipeFrom "git" (gitCommandLine repo params)

{- Scans for files that are checked into git at the specified locations. -}
inRepo :: Repo -> [FilePath] -> IO [FilePath]
inRepo repo l = pipeNullSplit repo $
	["ls-files", "--cached", "--exclude-standard", "-z", "--"] ++ l

{- Scans for files at the specified locations that are not checked into git,
 - and not gitignored. -}
notInRepo :: Repo -> [FilePath] -> IO [FilePath]
notInRepo repo l = pipeNullSplit repo $
	["ls-files", "--others", "--exclude-standard", "-z", "--"] ++ l

{- Returns a list of the files, staged for commit, that are being added,
 - moved, or changed (but not deleted), from the specified locations. -}
stagedFiles :: Repo -> [FilePath] -> IO [FilePath]
stagedFiles repo l = pipeNullSplit repo $
	["diff", "--cached", "--name-only", "--diff-filter=ACMRT", "-z", 
		"--"] ++ l

{- Returns a list of the files in the specified locations that are staged
 - for commit, and whose type has changed. -}
typeChangedStagedFiles :: Repo -> [FilePath] -> IO [FilePath]
typeChangedStagedFiles repo l = typeChangedFiles' repo l ["--cached"]

{- Returns a list of the files in the specified locations whose type has
 - changed.  Files only staged for commit will not be included. -}
typeChangedFiles :: Repo -> [FilePath] -> IO [FilePath]
typeChangedFiles repo l = typeChangedFiles' repo l []

typeChangedFiles' :: Repo -> [FilePath] -> [String] -> IO [FilePath]
typeChangedFiles' repo l middle = pipeNullSplit repo $ start ++ middle ++ end
	where
		start = ["diff", "--name-only", "--diff-filter=T", "-z"]
		end = ["--"] ++ l

{- Reads null terminated output of a git command (as enabled by the -z 
 - parameter), and splits it into a list of files. -}
pipeNullSplit :: Repo -> [String] -> IO [FilePath]
pipeNullSplit repo params = do
	-- XXX handle is left open, this is ok for git-annex, but may need
	-- to be cleaned up for other uses.
	(_, fs0) <- hPipeRead repo params
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
configRead r = assertLocal r $ error "internal"

{- Reads git config from a handle and populates a repo with it. -}
hConfigRead :: Repo -> Handle -> IO Repo
hConfigRead repo h = do
	val <- hGetContentsStrict h
	return $ configStore repo val

{- Parses a git config and returns a version of the repo using it. -}
configStore :: Repo -> String -> Repo
configStore repo s = r { remotes = configRemotes r }
	where r = repo { config = configParse s }

{- Checks if a string fron git config is a true value. -}
configTrue :: String -> Bool
configTrue s = map toLower s == "true"

{- Calculates a list of a repo's configured remotes, by parsing its config. -}
configRemotes :: Repo -> [Repo]
configRemotes repo = map construct remotepairs
	where
		remotepairs = Map.toList $ filterremotes $ config repo
		filterremotes = Map.filterWithKey (\k _ -> isremote k)
		isremote k = startswith "remote." k && endswith ".url" k
		remotename k = split "." k !! 1
		construct (k,v) = (gen v) { remoteName = Just $ remotename k }
		gen v	| scpstyle v = repoFromUrl $ scptourl v
			| isURI v = repoFromUrl v
			| otherwise = repoFromPath v
		-- git remotes can be written scp style -- [user@]host:dir
		-- where dir is relative to the user's home directory.
		scpstyle v = isInfixOf ":" v && (not $ isInfixOf "//" v)
		scptourl v = "ssh://" ++ host ++ slash dir
			where
				bits = split ":" v
				host = bits !! 0
				dir = join ":" $ drop 1 bits
				slash d	| d == "" = "/~/" ++ dir
					| d !! 0 == '/' = dir
					| otherwise = "/~/" ++ dir

{- Parses git config --list output into a config map. -}
configParse :: String -> Map.Map String String
configParse s = Map.fromList $ map pair $ lines s
	where
		pair l = (key l, val l)
		key l = head $ keyval l
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

{- Efficiently looks up a gitattributes value for each file in a list. -}
checkAttr :: Repo -> String -> [FilePath] -> IO [(FilePath, String)]
checkAttr repo attr files = do
	-- git check-attr wants files that are absolute (or relative to the
	-- top of the repo). But we're passed files relative to the current
	-- directory. Convert to absolute, and then convert the filenames
	-- in its output back to relative.
	absfiles <- mapM absPath files
	(_, s) <- pipeBoth "git" params $ join "\0" absfiles
	cwd <- getCurrentDirectory
	return $ map (topair $ cwd++"/") $ lines s
	-- XXX handle is left open, this is ok for git-annex, but may need
	-- to be cleaned up for other uses.
	where
		params = gitCommandLine repo ["check-attr", attr, "-z", "--stdin"]
		topair cwd l = (relfile, value)
			where 
				relfile 
					| startswith cwd file = drop (length cwd) file
					| otherwise = file
				file = decodeGitFile $ join sep $ take end bits
				value = bits !! end
				end = length bits - 1
				bits = split sep l
				sep = ": " ++ attr ++ ": "

{- Some git commands output encoded filenames. Decode that (annoyingly
 - complex) encoding. -}
decodeGitFile :: String -> FilePath
decodeGitFile [] = []
decodeGitFile f@(c:s)
	-- encoded strings will be inside double quotes
	| c == '"' = unescape ("", middle)
	| otherwise = f
	where
		e = '\\'
		middle = take (length s - 1) s
		unescape (b, []) = b
		-- look for escapes starting with '\'
		unescape (b, v) = b ++ beginning ++ unescape (decode rest)
			where
				pair = span (/= e) v
				beginning = fst pair
				rest = snd pair
		isescape x = x == e
		-- \NNN is an octal encoded character
		decode (x:n1:n2:n3:rest)
			| isescape x && alloctal = (fromoctal, rest)
				where
					alloctal = isOctDigit n1 &&
						isOctDigit n2 &&
						isOctDigit n3
					fromoctal = [chr $ readoctal [n1, n2, n3]]
					readoctal o = read $ "0o" ++ o :: Int
		-- \C is used for a few special characters
		decode (x:nc:rest)
			| isescape x = ([echar nc], rest)
			where
				echar 'a' = '\a'
				echar 'b' = '\b'
				echar 'f' = '\f'
				echar 'n' = '\n'
				echar 'r' = '\r'
				echar 't' = '\t'
				echar 'v' = '\v'
				echar a = a
		decode n = ("", n)

{- Should not need to use this, except for testing decodeGitFile. -}
encodeGitFile :: FilePath -> String
encodeGitFile s = foldl (++) "\"" (map echar s) ++ "\""
	where
		e c = '\\' : [c]
		echar '\a' = e 'a'
		echar '\b' = e 'b'
		echar '\f' = e 'f'
		echar '\n' = e 'n'
		echar '\r' = e 'r'
		echar '\t' = e 't'
		echar '\v' = e 'v'
		echar '\\' = e '\\'
		echar '"'  = e '"'
		echar x
			| ord x < 0x20 = e_num x -- low ascii
			| ord x >= 256 = e_utf x
			| ord x > 0x7E = e_num x -- high ascii
			| otherwise = [x]        -- printable ascii
			where 
				showoctal i = '\\' : printf "%03o" i
				e_num c = showoctal $ ord c
				-- unicode character is decomposed to
				-- Word8s and each is shown in octal
				e_utf c = foldl (++) "" $ map showoctal $
						(encode [c] :: [Word8])


{- for quickcheck -}
prop_idempotent_deencode :: String -> Bool
prop_idempotent_deencode s = s == decodeGitFile (encodeGitFile s)

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

isRepoTop :: FilePath -> IO Bool
isRepoTop dir = do
	r <- isRepo
	b <- isBareRepo
	return (r || b)
	where
		isRepo = gitSignature ".git" ".git/config"
		isBareRepo = gitSignature "objects" "config"
		gitSignature subdir file = do
			s <- (doesDirectoryExist (dir ++ "/" ++ subdir))
			f <- (doesFileExist (dir ++ "/" ++ file))
			return (s && f)
