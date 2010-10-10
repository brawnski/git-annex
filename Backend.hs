{- git-annex key/value storage backends
 -
 - git-annex uses a key/value abstraction layer to allow files contents to be
 - stored in different ways. In theory, any key/value storage system could be
 - used to store the file contents, and git-annex would then retrieve them
 - as needed and put them in `.git/annex/`.
 - 
 - When a file is annexed, a key is generated from its content and/or metadata.
 - This key can later be used to retrieve the file's content (its value). This
 - key generation must be stable for a given file content, name, and size.
 - 
 - The mapping from filename to its key is stored in the .git-annex directory,
 - in a file named `$filename.$backend`
 - 
 - Multiple pluggable backends are supported, and more than one can be used
 - to store different files' contents in a given repository.
 - -}

module Backend where

import GitRepo
import System.Directory

data Backend = Backend {
	name :: String,				-- name of this backend
	keyvalue :: FilePath -> Maybe String,	-- maps from key to value
	retrievekey :: IO String -> IO (Bool)	-- retrieves value given key
}

{- Name of state file that holds the key for an annexed file,
 - using a given backend. -}
backendFile :: Backend -> GitRepo -> FilePath -> String
backendFile backend repo file = gitStateDir repo ++
	(gitRelative repo file) ++ "." ++ (name backend)

{- Looks up the backend used for an already annexed file. -}
lookupBackend :: [Backend] -> GitRepo -> FilePath -> IO (Maybe Backend)
lookupBackend [] repo file = return Nothing
lookupBackend (b:bs) repo file = do
	present <- checkBackend b repo file
	if present
		then
			return $ Just b
		else
			lookupBackend bs repo file

{- Checks if a file is available via a given backend. -}
checkBackend :: Backend -> GitRepo -> FilePath -> IO (Bool)
checkBackend backend repo file = doesFileExist $ backendFile backend repo file

{- Attempts to retrieve an annexed file from one of the backends. -}
retrieveFile :: [Backend] -> GitRepo -> FilePath -> IO (Bool)
retrieveFile backends repo file = do
	result <- lookupBackend backends repo file
	case (result) of
		Nothing -> return False
		Just b -> (retrievekey b) key
			where key = readFile (backendFile b repo file)
