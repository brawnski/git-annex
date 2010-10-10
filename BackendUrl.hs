{- git-annex "url" backend
 - -}

module BackendUrl (backend) where

import Backend
import GitRepo

backend = Backend {
	name = "url",
	getKey = keyValue,
	storeFileKey = dummyStore,
	retrieveKeyFile = downloadUrl
}

-- cannot generate url from filename
keyValue :: GitRepo -> FilePath -> IO (Maybe Key)
keyValue repo file = return Nothing

-- cannot store to urls
dummyStore :: GitRepo -> FilePath -> Key -> IO (Bool)
dummyStore repo file url = return False

downloadUrl :: IO Key -> FilePath -> IO (Bool)
downloadUrl url file = error "downloadUrl unimplemented"
