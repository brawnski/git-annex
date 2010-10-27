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
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Backend (
	list,
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

import Locations
import qualified GitRepo as Git
import qualified Annex
import Utility
import Types
import qualified TypeInternals as Internals

{- List of backends in the order to try them when storing a new key. -}
list :: Annex [Backend]
list = do
	l <- Annex.backends -- list is cached here
	if (not $ null l)
		then return l
		else do
			all <- Annex.supportedBackends
			g <- Annex.gitRepo
			let l = parseBackendList all $ Git.configGet g "annex.backends" ""
			backendflag <- Annex.flagGet "backend"
			let l' = if (not $ null backendflag)
				then (lookupBackendName all backendflag):l
				else l
			Annex.backendsChange $ l'
			return l'
	where
		parseBackendList all s = 
			if (null s)
				then all
				else map (lookupBackendName all) $ words s

{- Looks up a backend in a list -}
lookupBackendName :: [Backend] -> String -> Backend
lookupBackendName all s =
	if ((length matches) /= 1)
		then error $ "unknown backend " ++ s
		else matches !! 0
	where matches = filter (\b -> s == Internals.name b) all

{- Attempts to store a file in one of the backends. -}
storeFileKey :: FilePath -> Annex (Maybe (Key, Backend))
storeFileKey file = do
	g <- Annex.gitRepo
	let relfile = Git.relative g file
	b <- list
	storeFileKey' b file relfile
storeFileKey' [] _ _ = return Nothing
storeFileKey' (b:bs) file relfile = do
	try <- (Internals.getKey b) relfile
	case (try) of
		Nothing -> nextbackend
		Just key -> do
			stored <- (Internals.storeFileKey b) file key
			if (not stored)
				then nextbackend
				else do
					return $ Just (key, b)
	where
		nextbackend = storeFileKey' bs file relfile

{- Attempts to retrieve an key from one of the backends, saving it to
 - a specified location. -}
retrieveKeyFile :: Backend -> Key -> FilePath -> Annex Bool
retrieveKeyFile backend key dest = (Internals.retrieveKeyFile backend) key dest

{- Removes a key from a backend. -}
removeKey :: Backend -> Key -> Annex Bool
removeKey backend key = (Internals.removeKey backend)  key

{- Checks if a backend has its key. -}
hasKey :: Key -> Annex Bool
hasKey key = do
	all <- Annex.supportedBackends
	(Internals.hasKey (lookupBackendName all $ backendName key)) key

{- Looks up the key and backend corresponding to an annexed file,
 - by examining what the file symlinks to. -}
lookupFile :: FilePath -> Annex (Maybe (Key, Backend))
lookupFile file = do
	all <- Annex.supportedBackends
	result <- liftIO $ (try (lookup all)::IO (Either SomeException (Maybe (Key, Backend))))
	case (result) of
		Left err -> return Nothing
		Right succ -> return succ
	where 
		lookup all = do
			l <- readSymbolicLink file
			return $ Just $ pair all $ takeFileName l
		pair all file = (k, b)
			where
				k = fileKey file
				b = lookupBackendName all $ backendName k
