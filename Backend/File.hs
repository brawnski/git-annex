{- git-annex pseudo-backend
 -
 - This backend does not really do any independant data storage,
 - it relies on the file contents in .git/annex/ in this repo,
 - and other accessible repos.
 -
 - This is an abstract backend; getKey has to be implemented to complete
 - it.
 -}

module Backend.File (backend) where

import Control.Monad.State
import System.IO
import System.Cmd
import System.Exit
import Control.Exception
import BackendTypes
import LocationLog
import Locations
import qualified Remotes
import qualified GitRepo as Git
import Utility
import Core
import qualified Annex
import UUID

backend = Backend {
	name = mustProvide,
	getKey = mustProvide,
	storeFileKey = dummyStore,
	retrieveKeyFile = copyKeyFile,
	removeKey = dummyRemove,
	hasKey = checkKeyFile
}

mustProvide = error "must provide this field"

{- Storing a key is a no-op. -}
dummyStore :: FilePath -> Key -> Annex (Bool)
dummyStore file key = return True

{- Allow keys to be removed. -}
dummyRemove :: Key -> Annex Bool
dummyRemove url = return True

{- Just check if the .git/annex/ file for the key exists. -}
checkKeyFile :: Key -> Annex Bool
checkKeyFile k = inAnnex k

{- Try to find a copy of the file in one of the remotes,
 - and copy it over to this one. -}
copyKeyFile :: Key -> FilePath -> Annex (Bool)
copyKeyFile key file = do
	remotes <- Remotes.withKey key
	if (0 == length remotes)
		then cantfind
		else return ()
	trycopy remotes remotes
	where
		trycopy full [] = error $ "unable to get: " ++ (keyFile key) ++ "\n" ++
			"To get that file, need access to one of these remotes: " ++
			(Remotes.list full)
		trycopy full (r:rs) = do
			-- annexLocation needs the git config to have been
			-- read for a remote, so do that now,
			-- if it hasn't been already
			result <- Remotes.tryGitConfigRead r
			case (result) of
				Nothing -> trycopy full rs
				Just r' -> do
					result <- liftIO $ (try (copyFromRemote r' key file)::IO (Either SomeException ()))
		        		case (result) of
				                Left err -> do
							liftIO $ hPutStrLn stderr (show err)
							trycopy full rs
				                Right succ -> return True
		cantfind = do
			g <- Annex.gitRepo
			uuids <- liftIO $ keyLocations g key
			ppuuids <- prettyPrintUUIDs uuids
			error $ "no available git remotes have: " ++
				(keyFile key) ++ 
				if (0 < length uuids)
					then "\nIt has been seen before in these repositories:\n" ++ ppuuids
					else ""

{- Tries to copy a file from a remote, exception on error. -}
copyFromRemote :: Git.Repo -> Key -> FilePath -> IO ()
copyFromRemote r key file = do
	putStrLn $ "copy from " ++ (Git.repoDescribe r ) ++ " " ++ file

	if (Git.repoIsLocal r)
		then getlocal
		else getremote
	where
		getlocal = do
			res <-rawSystem "cp" ["-a", location, file]
			if (res == ExitSuccess)
				then return ()
				else error "cp failed"
		getremote = error "get via network not yet implemented!"
		location = annexLocation r key
