{- git-annex "url" backend
 - -}

module BackendUrl (backend) where

import Backend

backend = Backend {
	name = "url",
	keyvalue = keyValue,
	retrievekey = downloadUrl
}

-- cannot generate url from filename
keyValue k = Nothing

downloadUrl k = error "unimplemented"
