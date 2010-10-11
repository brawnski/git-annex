{- git-annex toplevel code
 -}

module Annex (
	State,
	startAnnex,
	annexFile,
	unannexFile
) where

import System.Posix.Files
import System.Directory
import GitRepo
import Utility
import Locations
import Backend
import BackendList
import LocationLog

-- git-annex's runtime state
data State = State {
	repo :: GitRepo,
	config :: Config
}

data Config = Config {
	annex_name :: String,
	annex_numcopies :: Int,
	annex_backends :: [Backend]
}

{- An annexed file's content is stored somewhere under .git/annex/ -}
annexDir :: GitRepo -> Key -> IO FilePath
annexDir repo key = do
	dir <- gitDir repo
	return $ dir ++ "/annex/" ++ key

{- On startup, examine the git repo, prepare it, and record state for
 - later. -}
startAnnex :: IO State
startAnnex = do
	r <- gitRepoCurrent
	config <- queryConfig r
	gitPrep r
	return State {
		repo = r,
		config = config
	}

{- Annexes a file, storing it in a backend, and then moving it into
 - the annex directory and setting up the symlink pointing to its content. -}
annexFile :: State -> FilePath -> IO ()
annexFile state file = do
	alreadyannexed <- lookupBackend backends (repo state) file
	case (alreadyannexed) of
		Just _ -> error $ "already annexed: " ++ file
		Nothing -> do
			checkLegal file
			stored <- storeFile backends (repo state) file
			case (stored) of
				Nothing -> error $ "no backend could store: " ++ file
				Just key -> symlink key
	where
		symlink key = do
			dest <- annexDir (repo state) key
			createDirectoryIfMissing True (parentDir dest)
			renameFile file dest
			createSymbolicLink dest file
			gitAdd (repo state) file
		checkLegal file = do
			s <- getSymbolicLinkStatus file
			if ((isSymbolicLink s) || (not $ isRegularFile s))
				then error $ "not a regular file: " ++ file
				else return ()
		backends = getConfig state annex_backends

{- Inverse of annexFile. -}
unannexFile :: State -> FilePath -> IO ()
unannexFile state file = do
	alreadyannexed <- lookupBackend backends (repo state) file
	case (alreadyannexed) of
		Nothing -> error $ "not annexed " ++ file
		Just _ -> do
			mkey <- dropFile backends (repo state) file
			case (mkey) of
				Nothing -> return ()
				Just key -> do
					src <- annexDir (repo state) key
					removeFile file
					renameFile src file
					return ()
	where
		backends = getConfig state annex_backends

{- Query the git repo for relevant configuration settings. -}
queryConfig :: GitRepo -> IO Config
queryConfig repo = do
	-- a name can be configured, if none is, use the repository path
	name <- gitConfigGet "annex.name" (gitRepoTop repo)
	-- default number of copies to keep of file contents is 1
	numcopies <- gitConfigGet "annex.numcopies" "1"
	backends <- gitConfigGet "annex.backends" ""
	
	return Config {
		annex_name = name,
		annex_numcopies = read numcopies,
		annex_backends = parseBackendList backends
	}

{- Sets up a git repo for git-annex. May be called repeatedly. -}
gitPrep :: GitRepo -> IO ()
gitPrep repo = do
	-- configure git to use union merge driver on state files
	let attrLine = stateLoc ++ "/*.log merge=union"
	attributes <- gitAttributes repo
	exists <- doesFileExist attributes
	if (not exists)
		then do
			writeFile attributes $ attrLine ++ "\n"
			gitAdd repo attributes
		else do
			content <- readFile attributes
			if (all (/= attrLine) (lines content))
				then do
					appendFile attributes $ attrLine ++ "\n"
					gitAdd repo attributes
				else return ()

{- Looks up a key in a State's Config -}
getConfig :: State -> (Config -> b) -> b
getConfig state key = key $ config state
