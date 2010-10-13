{- git-annex "file" backend
 - -}

module BackendFile (backend) where

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
			ok <- copyFromRemote r key file
			if (ok)
				then return True
				else trycopy full rs

{- Tries to copy a file from a remote. -}
copyFromRemote :: GitRepo -> Key -> FilePath -> IO (Bool)
copyFromRemote r key file = do
	return False -- TODO	
