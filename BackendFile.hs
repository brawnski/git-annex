{- git-annex "file" backend
 - -}

module BackendFile (backend) where

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
keyValue :: State -> FilePath -> IO (Maybe Key)
keyValue state file = return $ Just $ Key file

{- This backend does not really do any independant data storage,
 - it relies on the file contents in .git/annex/ in this repo,
 - and other accessible repos. So storing or removing a key is
 - a no-op. TODO until support is added for git annex --push otherrepo,
 - then these could implement that.. -}
dummyStore :: State -> FilePath -> Key -> IO (Bool)
dummyStore state file key = return True
dummyRemove :: State -> Key -> IO Bool
dummyRemove state url = return False

{- Try to find a copy of the file in one of the remotes,
 - and copy it over to this one. -}
copyKeyFile :: State -> Key -> FilePath -> IO (Bool)
copyKeyFile state key file = do
	remotes <- remotesWithKey state key
	if (0 == length remotes)
		then error $ "no known remotes have: " ++ (keyFile key) ++ "\n" ++
			"(Perhaps you need to git remote add a repository?)"
		else trycopy remotes remotes
	where
		trycopy full [] = error $ "unable to get: " ++ (keyFile key) ++ "\n" ++
			"To get that file, need access to one of these remotes: " ++
			(remotesList full)
		trycopy full (r:rs) = do
			putStrLn "trying a remote"
			result <- try (copyFromRemote r key file)::IO (Either SomeException ())
        		case (result) of
		                Left err -> do
					showerr err r
					trycopy full rs
		                Right succ -> return True
		showerr err r = do
			hPutStrLn stderr $ "git-annex: copy from " ++ 
				(gitRepoDescribe r ) ++ " failed: " ++
				(show err)

{- Tries to copy a file from a remote, exception on error. -}
copyFromRemote :: GitRepo -> Key -> FilePath -> IO ()
copyFromRemote r key file = do
	r <- if (gitRepoIsLocal r)
		then getlocal
		else getremote
	return ()
	where
		getlocal = do
			putStrLn $ "get: " ++ location
			rawSystem "cp" ["-a", location, file]
		getremote = do
			putStrLn $ "get: " ++ location
			error "get via network not yet implemented!"
		location = annexLocation r backend key
