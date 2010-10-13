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
	storeFile,
	dropFile,
	retrieveFile,
	lookupFile
) where

import Control.Exception
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
retrieveFile :: State -> Backend -> Key -> FilePath -> IO Bool
retrieveFile state backend key dest = (retrieveKeyFile backend) state key dest

{- Drops a key from a backend. -}
dropFile :: State -> Backend -> Key -> IO Bool
dropFile state backend key = (removeKey backend) state key

{- Looks up the key and backend corresponding to an annexed file,
 - by examining what the file symlinks to. -}
lookupFile :: FilePath -> IO (Maybe (Key, Backend))
lookupFile file = do
	result <- try (lookup)::IO (Either SomeException (Maybe (Key, Backend)))
	case (result) of
		Left err -> return Nothing
		Right succ -> return succ
	where 
		lookup = do
			l <- readSymbolicLink file
			return $ Just (k l, b l)
		k l = fileKey $ takeFileName $ l
		b l = lookupBackendName $ takeFileName $ parentDir $ l
