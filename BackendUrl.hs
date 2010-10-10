{- git-annex "url" backend
 - -}

module BackendUrl (backend) where

import Backend

backend = Backend {
	name = "url",
	getKey = keyValue,
	storeFileKey = dummyStore,
	retrieveKeyFile = downloadUrl
}

-- cannot generate url from filename
keyValue :: FilePath -> IO (Maybe Key)
keyValue k = return Nothing

-- cannot store to urls
dummyStore :: FilePath -> Key -> IO (Bool)
dummyStore file url = return False

downloadUrl :: IO Key -> FilePath -> IO (Bool)
downloadUrl url file = error "unimplemented"
