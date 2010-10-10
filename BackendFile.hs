{- git-annex "file" backend
 - -}

module BackendFile (backend) where

import Backend

backend = Backend {
	name = "file",
	getKey = keyValue,
	storeFileKey = moveToAnnex,
	retrieveKeyFile = copyFromOtherRepo
}

-- direct mapping from filename to key
keyValue :: FilePath -> IO (Maybe Key)
keyValue k = return $ Just $ id k

moveToAnnex :: FilePath -> Key -> IO (Bool)
moveToAnnex file key = return False

copyFromOtherRepo :: IO Key -> FilePath -> IO (Bool)
copyFromOtherRepo key file = return False

