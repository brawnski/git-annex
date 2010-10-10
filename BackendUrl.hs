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
keyValue :: GitRepo -> FilePath -> IO (Maybe Key)
keyValue repo file = return Nothing

-- cannot change urls
dummyStore :: GitRepo -> FilePath -> Key -> IO Bool
dummyStore repo file url = return False
dummyRemove :: Key -> IO Bool
dummyRemove url = return False

downloadUrl :: Key -> FilePath -> IO Bool
downloadUrl url file = error "downloadUrl unimplemented"
