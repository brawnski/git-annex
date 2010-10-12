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

module Backend (
	Key,
	Backend, -- note only data type is exported, not destructors
	lookupBackend,
	storeFile,
	dropFile
) where

import System.Directory
import Locations
import GitRepo
import Utility
import Types

{- Name of state file that holds the key for an annexed file,
 - using a given backend. -}
backendFile :: Backend -> State -> FilePath -> String
backendFile backend state file =
	gitStateDir (repo state) ++ (gitRelative (repo state) file) ++ 
		"." ++ (name backend)

{- Attempts to store a file in one of the backends, and returns
 - its key. -}
storeFile :: [Backend] -> State -> FilePath -> IO (Maybe Key)
storeFile [] _ _ = return Nothing
storeFile (b:bs) state file = do
	try <- (getKey b) state (gitRelative (repo state) file)
	case (try) of
		Nothing -> nextbackend
		Just key -> do
			stored <- (storeFileKey b) state file key
			if (not stored)
				then nextbackend
				else do
					bookkeeping key
					return $ Just key
	where
		nextbackend = storeFile bs state file
		backendfile = backendFile b state file
		bookkeeping key = do
			createDirectoryIfMissing True (parentDir backendfile)
			writeFile backendfile key

{- Attempts to retrieve an file from one of the backends, saving it to
 - a specified location. -}
retrieveFile :: [Backend] -> State -> FilePath -> FilePath -> IO Bool
retrieveFile backends state file dest = do
	result <- lookupBackend backends state file
	case (result) of
		Nothing -> return False
		Just b -> do
			key <- lookupKey b state file
			(retrieveKeyFile b) state key dest

{- Drops the key for a file from the backend that has it. -}
dropFile :: [Backend] -> State -> FilePath -> IO (Maybe Key)
dropFile backends state file = do
	result <- lookupBackend backends state file
	case (result) of
		Nothing -> return Nothing
		Just b -> do
			key <- lookupKey b state file
			(removeKey b) state key
			removeFile $ backendFile b state file
			return $ Just key

{- Looks up the key a backend uses for an already annexed file. -}
lookupKey :: Backend -> State -> FilePath -> IO Key
lookupKey backend state file = readFile (backendFile backend state file)

{- Looks up the backend used for an already annexed file. -}
lookupBackend :: [Backend] -> State -> FilePath -> IO (Maybe Backend)
lookupBackend [] _ _ = return Nothing
lookupBackend (b:bs) state file = do
	present <- checkBackend b state file
	if present
		then
			return $ Just b
		else
			lookupBackend bs state file

{- Checks if a file is available via a given backend. -}
checkBackend :: Backend -> State -> FilePath -> IO (Bool)
checkBackend backend state file = doesFileExist $ backendFile backend state file
