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

import System.Directory
import GitRepo
import Utility

type Key = String

data Backend = Backend {
	-- name of this backend
	name :: String,
	-- converts a filename to a key
	getKey :: FilePath -> IO (Maybe Key),
	-- stores a file's contents to a key
	storeFileKey :: FilePath -> Key -> IO (Bool),
	-- retrieves a key's contents to a file
	retrieveKeyFile :: IO Key -> FilePath -> IO (Bool)
}

instance Show Backend where
	show backend = "Backend { name =\"" ++ (name backend) ++ "\" }"

{- Name of state file that holds the key for an annexed file,
 - using a given backend. -}
backendFile :: Backend -> GitRepo -> FilePath -> String
backendFile backend repo file = gitStateDir repo ++
	(gitRelative repo file) ++ "." ++ (name backend)

{- Attempts to store a file in one of the backends, and returns
 - its key. -}
storeFile :: [Backend] -> GitRepo -> FilePath -> IO (Maybe Key)
storeFile [] _ _ = return Nothing
storeFile (b:bs) repo file = do
	try <- (getKey b) (gitRelative repo file)
	case (try) of
		Nothing -> nextbackend
		Just key -> do
			stored <- (storeFileKey b) file key
			if (not stored)
				then nextbackend
				else do
					bookkeeping key
					return $ Just key
	where
		nextbackend = storeFile bs repo file
		backendfile = backendFile b repo file
		bookkeeping key = do
			createDirectoryIfMissing True (parentDir backendfile)
			writeFile backendfile key

{- Attempts to retrieve an file from one of the backends, saving it to
 - a specified location. -}
retrieveFile :: [Backend] -> GitRepo -> FilePath -> FilePath -> IO (Bool)
retrieveFile backends repo file dest = do
	result <- lookupBackend backends repo file
	case (result) of
		Nothing -> return False
		Just b -> (retrieveKeyFile b) key dest
			where
				key = readFile (backendFile b repo file)

{- Looks up the backend used for an already annexed file. -}
lookupBackend :: [Backend] -> GitRepo -> FilePath -> IO (Maybe Backend)
lookupBackend [] _ _ = return Nothing
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
