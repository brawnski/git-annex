{- git-annex "file" backend
 - -}

module BackendFile (backend) where

import Control.Monad.State
import System.IO
import System.Cmd
import Control.Exception
import Types
import LocationLog
import Locations
import Remotes
import GitRepo

backend = Backend {
	name = "file",
	getKey = keyValue,
	storeFileKey = dummyStore,
	retrieveKeyFile = copyKeyFile,
	removeKey = dummyRemove
}

-- direct mapping from filename to key
keyValue :: FilePath -> Annex (Maybe Key)
keyValue file = return $ Just $ Key file

{- This backend does not really do any independant data storage,
 - it relies on the file contents in .git/annex/ in this repo,
 - and other accessible repos. So storing or removing a key is
 - a no-op. TODO until support is added for git annex --push otherrepo,
 - then these could implement that.. -}
dummyStore :: FilePath -> Key -> Annex (Bool)
dummyStore file key = return True
dummyRemove :: Key -> Annex Bool
dummyRemove url = return False

{- Try to find a copy of the file in one of the remotes,
 - and copy it over to this one. -}
copyKeyFile :: Key -> FilePath -> Annex (Bool)
copyKeyFile key file = do
	remotes <- remotesWithKey key
	trycopy remotes remotes
	where
		trycopy full [] = error $ "unable to get: " ++ (keyFile key) ++ "\n" ++
			"To get that file, need access to one of these remotes: " ++
			(remotesList full)
		trycopy full (r:rs) = do
			-- annexLocation needs the git config to have been
			-- read for a remote, so do that now,
			-- if it hasn't been already
			r' <- remoteEnsureGitConfigRead r
			result <- liftIO $ (try (copyFromRemote r' key file)::IO (Either SomeException ()))
        		case (result) of
		                Left err -> do
					liftIO $ hPutStrLn stderr (show err)
					trycopy full rs
		                Right succ -> return True

{- Tries to copy a file from a remote, exception on error. -}
copyFromRemote :: GitRepo -> Key -> FilePath -> IO ()
copyFromRemote r key file = do
	putStrLn $ "copy from " ++ (gitRepoDescribe r ) ++ " " ++ file

	if (gitRepoIsLocal r)
		then getlocal
		else getremote
	return ()
	where
		getlocal = rawSystem "cp" ["-a", location, file]
		getremote = error "get via network not yet implemented!"
		location = annexLocation r backend key
