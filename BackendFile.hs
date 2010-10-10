{- git-annex "file" backend
 - -}

module BackendFile (backend) where

import Backend

backend = Backend {
	name = "file",
	keyvalue = keyValue,
	retrievekey = copyFile
}

-- direct mapping from filename to key
keyValue k = Just $ id k

copyFile f = error "unimplemented"
