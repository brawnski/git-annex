{- git-annex "url" backend
 - -}

module BackendUrl (backend) where

import Types

backend = Backend {
	name = "url",
	getKey = keyValue,
	storeFileKey = dummyStore,
	retrieveKeyFile = downloadUrl,
	removeKey = dummyRemove
}

-- cannot generate url from filename
keyValue :: State -> FilePath -> IO (Maybe Key)
keyValue repo file = return Nothing

-- cannot change urls
dummyStore :: State -> FilePath -> Key -> IO Bool
dummyStore repo file url = return False
dummyRemove :: State -> Key -> IO Bool
dummyRemove state url = return False

downloadUrl :: State -> Key -> FilePath -> IO Bool
downloadUrl state url file = error "downloadUrl unimplemented"
