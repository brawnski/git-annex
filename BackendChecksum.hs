{- git-annex "checksum" backend
 - -}

module BackendChecksum (backend) where

import Backend
import qualified BackendFile
import Data.Digest.Pure.SHA

-- based on BackendFile just with a different key type
backend = BackendFile.backend {
	name = "checksum",
	getKey = keyValue
}

-- 
keyValue :: FilePath -> IO (Maybe Key)
keyValue k = error "unimplemented" -- TODO
