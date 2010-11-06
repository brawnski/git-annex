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
	lookupFile,
	chooseBackends
) where

import Control.Monad.State
import IO (try)
import System.FilePath
import System.Posix.Files
import Core

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
			Annex.backendsChange l'
			return l'
	where
		parseBackendList bs s = 
			if (null s)
				then bs
				else map (lookupBackendName bs) $ words s

{- Looks up a backend in a list. May fail if unknown. -}
lookupBackendName :: [Backend] -> String -> Backend
lookupBackendName bs s =
	case maybeLookupBackendName bs s of
		Just b -> b
		Nothing -> error $ "unknown backend " ++ s
maybeLookupBackendName :: [Backend] -> String -> Maybe Backend
maybeLookupBackendName bs s =
	if ((length matches) /= 1)
		then Nothing
		else Just $ head matches
	where matches = filter (\b -> s == Internals.name b) bs

{- Attempts to store a file in one of the backends. -}
storeFileKey :: FilePath -> Maybe Backend -> Annex (Maybe (Key, Backend))
storeFileKey file trybackend = do
	g <- Annex.gitRepo
	let relfile = Git.relative g file
	bs <- list
	let bs' = case trybackend of
		Nothing -> bs
		Just backend -> backend:bs
	storeFileKey' bs' file relfile
storeFileKey' :: [Backend] -> FilePath -> FilePath -> Annex (Maybe (Key, Backend))
storeFileKey' [] _ _ = return Nothing
storeFileKey' (b:bs) file relfile = do
	result <- (Internals.getKey b) relfile
	case result of
		Nothing -> nextbackend
		Just key -> do
			stored <- (Internals.storeFileKey b) file key
			if (not stored)
				then nextbackend
				else return $ Just (key, b)
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
	tl <- liftIO $ try getsymlink
	case tl of
		Left _ -> return Nothing
		Right l -> makekey bs l
	where
		getsymlink = do
			l <- readSymbolicLink file
			return $ takeFileName l
		makekey bs l =
			case maybeLookupBackendName bs bname of
				Nothing -> do
					unless (null kname || null bname) $
						warning skip
					return Nothing
				Just backend -> return $ Just (k, backend)
			where
				k = fileKey l
				bname = backendName k
				kname = keyName k
				skip = "skipping " ++ file ++ 
					" (unknown backend " ++ bname ++ ")"

{- Looks up the backends that should be used for each file in a list.
 - That can be configured on a per-file basis in the gitattributes file.
 -}
chooseBackends :: [FilePath] -> Annex [(FilePath, Maybe Backend)]
chooseBackends fs = do
	g <- Annex.gitRepo
	bs <- Annex.supportedBackends
	pairs <- liftIO $ Git.checkAttr g "git-annex-backend" fs
	return $ map (\(f,b) -> (f, maybeLookupBackendName bs b)) pairs
