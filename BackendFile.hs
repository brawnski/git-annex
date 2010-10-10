{- git-annex "file" backend
 - -}

module BackendFile (backend) where

import Backend
import GitRepo

backend = Backend {
	name = "file",
	getKey = keyValue,
	storeFileKey = dummyStore,
	retrieveKeyFile = copyFromOtherRepo
}

-- direct mapping from filename to key
keyValue :: GitRepo -> FilePath -> IO (Maybe Key)
keyValue repo file = return $ Just file

-- This backend does not really do any independant data storage,
-- it relies on the file contents in .git/annex/ in this repo,
-- and other accessible repos. So storing a file is a no-op.
dummyStore :: GitRepo -> FilePath -> Key -> IO (Bool)
dummyStore repo file key = return True

copyFromOtherRepo :: IO Key -> FilePath -> IO (Bool)
copyFromOtherRepo key file = error "copyFromOtherRepo unimplemented" -- TODO
