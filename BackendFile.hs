{- git-annex "file" backend
 - -}

module BackendFile (backend) where

import Types

backend = Backend {
	name = "file",
	getKey = keyValue,
	storeFileKey = dummyStore,
	retrieveKeyFile = copyFromOtherRepo,
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

{- Try to find a copy of the file in one of the other repos,
 - and copy it over to this one. -}
copyFromOtherRepo :: State -> Key -> FilePath -> IO (Bool)
copyFromOtherRepo state key file =
	-- 1. get ordered list of remotes (local repos, then remote repos)
	-- 2. read locationlog for file
	-- 3. filter remotes list to ones that have file
	-- 4. attempt to transfer from each remote until success
	error "copyFromOtherRepo unimplemented" -- TODO
