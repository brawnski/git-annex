{- git-annex key-value storage backends
 -
 - git-annex uses a key-value abstraction layer to allow files contents to be
 - stored in different ways. In theory, any key-value storage system could be
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
	storeFileKey,
	retrieveKeyFile,
	removeKey,
	hasKey,
	lookupFile
) where

import Control.Monad.State
import Control.Exception
import System.Directory
import System.FilePath
import Data.String.Utils
import System.Posix.Files
import BackendList
import Locations
import qualified GitRepo as Git
import qualified Annex
import Utility
import Types
import qualified BackendTypes as B
import BackendList

{- List of backends in the order to try them when storing a new key. -}
backendList :: Annex [Backend]
backendList = do
	l <- Annex.backends
	if (0 < length l)
		then return l
		else do
			g <- Annex.gitRepo
			let l = parseBackendList $ Git.configGet g "annex.backends" ""
			Annex.backendsChange l
			return l

{- Attempts to store a file in one of the backends. -}
storeFileKey :: FilePath -> Annex (Maybe (Key, Backend))
storeFileKey file = do
	g <- Annex.gitRepo
	let relfile = Git.relative g file
	b <- backendList
	storeFileKey' b file relfile
storeFileKey' [] _ _ = return Nothing
storeFileKey' (b:bs) file relfile = do
	try <- (B.getKey b) relfile
	case (try) of
		Nothing -> nextbackend
		Just key -> do
			stored <- (B.storeFileKey b) file key
			if (not stored)
				then nextbackend
				else do
					return $ Just (key, b)
	where
		nextbackend = storeFileKey' bs file relfile

{- Attempts to retrieve an key from one of the backends, saving it to
 - a specified location. -}
retrieveKeyFile :: Backend -> Key -> FilePath -> Annex Bool
retrieveKeyFile backend key dest = (B.retrieveKeyFile backend) key dest

{- Removes a key from a backend. -}
removeKey :: Backend -> Key -> Annex Bool
removeKey backend key = (B.removeKey backend)  key

{- Checks if a backend has its key. -}
hasKey :: Key -> Annex Bool
hasKey key = (B.hasKey (lookupBackendName $ backendName key)) key

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
			return $ Just $ pair $ takeFileName l
		pair file = (k, b)
			where
				k = fileKey file
				b = lookupBackendName $ backendName k
