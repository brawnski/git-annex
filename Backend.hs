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
	fsckKey,
	lookupFile,
	chooseBackends,
	keyBackend
) where

import Control.Monad.State
import System.IO.Error (try)
import System.FilePath
import System.Posix.Files

import Locations
import qualified GitRepo as Git
import qualified Annex
import Types
import qualified BackendTypes as B
import Messages

{- List of backends in the order to try them when storing a new key. -}
list :: Annex [Backend Annex]
list = do
	l <- Annex.getState Annex.backends -- list is cached here
	if not $ null l
		then return l
		else do
			s <- getstandard
			d <- Annex.getState Annex.defaultbackend
			handle d s
	where
		parseBackendList l [] = l
		parseBackendList bs s = map (lookupBackendName bs) $ words s
		handle Nothing s = return s
		handle (Just "") s = return s
		handle (Just name) s = do
			bs <- Annex.getState Annex.supportedBackends
			let l' = (lookupBackendName bs name):s
			Annex.changeState $ \state -> state { Annex.backends = l' }
			return l'
		getstandard = do
			bs <- Annex.getState Annex.supportedBackends
			g <- Annex.gitRepo
			return $ parseBackendList bs $
				Git.configGet g "annex.backends" ""

{- Looks up a backend in a list. May fail if unknown. -}
lookupBackendName :: [Backend Annex] -> String -> Backend Annex
lookupBackendName bs s =
	case maybeLookupBackendName bs s of
		Just b -> b
		Nothing -> error $ "unknown backend " ++ s
maybeLookupBackendName :: [Backend Annex] -> String -> Maybe (Backend Annex)
maybeLookupBackendName bs s =
	if 1 /= length matches
		then Nothing
		else Just $ head matches
	where matches = filter (\b -> s == B.name b) bs

{- Attempts to store a file in one of the backends. -}
storeFileKey :: FilePath -> Maybe (Backend Annex) -> Annex (Maybe (Key, Backend Annex))
storeFileKey file trybackend = do
	bs <- list
	let bs' = case trybackend of
		Nothing -> bs
		Just backend -> backend:bs
	storeFileKey' bs' file
storeFileKey' :: [Backend Annex] -> FilePath -> Annex (Maybe (Key, Backend Annex))
storeFileKey' [] _ = return Nothing
storeFileKey' (b:bs) file = do
	result <- (B.getKey b) file
	case result of
		Nothing -> nextbackend
		Just key -> do
			stored <- (B.storeFileKey b) file key
			if (not stored)
				then nextbackend
				else return $ Just (key, b)
	where
		nextbackend = storeFileKey' bs file

{- Attempts to retrieve an key from one of the backends, saving it to
 - a specified location. -}
retrieveKeyFile :: Backend Annex -> Key -> FilePath -> Annex Bool
retrieveKeyFile backend key dest = (B.retrieveKeyFile backend) key dest

{- Removes a key from a backend. -}
removeKey :: Backend Annex -> Key -> Maybe Int -> Annex Bool
removeKey backend key numcopies = (B.removeKey backend) key numcopies

{- Checks if a key is present in its backend. -}
hasKey :: Key -> Annex Bool
hasKey key = do
	backend <- keyBackend key
	(B.hasKey backend) key

{- Checks a key's backend for problems. -}
fsckKey :: Backend Annex -> Key -> Maybe FilePath -> Maybe Int -> Annex Bool
fsckKey backend key file numcopies = (B.fsckKey backend) key file numcopies

{- Looks up the key and backend corresponding to an annexed file,
 - by examining what the file symlinks to. -}
lookupFile :: FilePath -> Annex (Maybe (Key, Backend Annex))
lookupFile file = do
	bs <- Annex.getState Annex.supportedBackends
	tl <- liftIO $ try getsymlink
	case tl of
		Left _ -> return Nothing
		Right l -> makekey bs l
	where
		getsymlink = do
			l <- readSymbolicLink file
			return $ takeFileName l
		makekey bs l = do
			case maybeLookupBackendName bs bname of
				Nothing -> do
					unless (null kname || null bname ||
					        not (isLinkToAnnex l)) $
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
chooseBackends :: [FilePath] -> Annex [(FilePath, Maybe (Backend Annex))]
chooseBackends fs = do
	g <- Annex.gitRepo
	bs <- Annex.getState Annex.supportedBackends
	pairs <- liftIO $ Git.checkAttr g "annex.backend" fs
	return $ map (\(f,b) -> (f, maybeLookupBackendName bs b)) pairs

{- Returns the backend to use for a key. -}
keyBackend :: Key -> Annex (Backend Annex)
keyBackend key = do
	bs <- Annex.getState Annex.supportedBackends
	return $ lookupBackendName bs $ backendName key
