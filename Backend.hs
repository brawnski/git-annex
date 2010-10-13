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
 - Multiple pluggable backends are supported, and more than one can be used
 - to store different files' contents in a given repository.
 - -}

module Backend (
	lookupBackend,
	storeFile,
	dropFile,
	retrieveFile,
	fileKey,
	fileBackend
) where

import System.Directory
import System.FilePath
import Data.String.Utils
import System.Posix.Files
import BackendList
import Locations
import GitRepo
import Utility
import Types

{- Attempts to store a file in one of the backends. -}
storeFile :: State -> FilePath -> IO (Maybe (Key, Backend))
storeFile state file = storeFile' (backends state) state file
storeFile' [] _ _ = return Nothing
storeFile' (b:bs) state file = do
	try <- (getKey b) state (gitRelative (repo state) file)
	case (try) of
		Nothing -> nextbackend
		Just key -> do
			stored <- (storeFileKey b) state file key
			if (not stored)
				then nextbackend
				else do
					return $ Just (key, b)
	where
		nextbackend = storeFile' bs state file

{- Attempts to retrieve an key from one of the backends, saving it to
 - a specified location. -}
retrieveFile :: State -> Key -> FilePath -> IO Bool
retrieveFile state key dest = do
	result <- lookupBackend state key
	case (result) of
		Nothing -> return False
		Just backend -> (retrieveKeyFile backend) state key dest

{- Drops a key from the backend that has it. -}
dropFile :: State -> Key -> IO (Maybe (Key, Backend))
dropFile state key = do
	result <- lookupBackend state key
	case (result) of
		Nothing -> return Nothing
		Just backend -> do
			(removeKey backend) state key
			return $ Just (key, backend)

{- Looks up the backend that has a key. -}
lookupBackend :: State -> Key -> IO (Maybe Backend)
lookupBackend state key = lookupBackend' (backends state) state key
lookupBackend' [] _ _ = return Nothing
lookupBackend' (b:bs) state key = do
	present <- checkBackend b state key
	if present
		then
			return $ Just b
		else
			lookupBackend' bs state key

{- Checks if a key is available via a given backend. -}
checkBackend :: Backend -> State -> Key -> IO (Bool)
checkBackend backend state key = 
	doesFileExist $ annexLocation state backend key

{- Looks up the key corresponding to an annexed file,
 - by examining what the file symlinks to. -}
fileKey :: FilePath -> IO Key
fileKey file = do
	l <- readSymbolicLink (file)
	return $ Key $ takeFileName $ l

{- Looks up the backend corresponding to an annexed file,
 - by examining what the file symlinks to. -}
fileBackend :: FilePath -> IO Backend
fileBackend file = do
	l <- readSymbolicLink (file)
	return $ lookupBackendName $ takeFileName $ parentDir $ l
