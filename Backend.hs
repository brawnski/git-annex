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
import Control.Exception.Extensible
import System.FilePath
import System.Posix.Files

import Locations
import qualified GitRepo as Git
import qualified Annex
import Types
import qualified TypeInternals as Internals

{- List of backends in the order to try them when storing a new key. -}
list :: Annex [Backend]
list = do
	l <- Annex.backends -- list is cached here
	if (not $ null l)
		then return l
		else do
			bs <- Annex.supportedBackends
			g <- Annex.gitRepo
			let defaults = parseBackendList bs $ Git.configGet g "annex.backends" ""
			backendflag <- Annex.flagGet "backend"
			let l' = if (not $ null backendflag)
				then (lookupBackendName bs backendflag):defaults
				else defaults
			Annex.backendsChange $ l'
			return l'
	where
		parseBackendList bs s = 
			if (null s)
				then bs
				else map (lookupBackendName bs) $ words s

{- Looks up a backend in a list -}
lookupBackendName :: [Backend] -> String -> Backend
lookupBackendName bs s =
	if ((length matches) /= 1)
		then error $ "unknown backend " ++ s
		else matches !! 0
	where matches = filter (\b -> s == Internals.name b) bs

{- Attempts to store a file in one of the backends. -}
storeFileKey :: FilePath -> Annex (Maybe (Key, Backend))
storeFileKey file = do
	g <- Annex.gitRepo
	let relfile = Git.relative g file
	b <- list
	storeFileKey' b file relfile
storeFileKey' :: [Backend] -> FilePath -> FilePath -> Annex (Maybe (Key, Backend))
storeFileKey' [] _ _ = return Nothing
storeFileKey' (b:bs) file relfile = do
	result <- (Internals.getKey b) relfile
	case (result) of
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
	bs <- Annex.supportedBackends
	(Internals.hasKey (lookupBackendName bs $ backendName key)) key

{- Looks up the key and backend corresponding to an annexed file,
 - by examining what the file symlinks to. -}
lookupFile :: FilePath -> Annex (Maybe (Key, Backend))
lookupFile file = do
	bs <- Annex.supportedBackends
	result <- liftIO $ (try (find bs)::IO (Either SomeException (Maybe (Key, Backend))))
	case (result) of
		Left _ -> return Nothing
		Right val -> return val
	where 
		find bs = do
			l <- readSymbolicLink file
			return $ Just $ pair bs $ takeFileName l
		pair bs f = (k, b)
			where
				k = fileKey f
				b = lookupBackendName bs $ backendName k
