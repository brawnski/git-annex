{- git-annex "checksum" backend
 - -}

module BackendChecksum (backend) where

import Types
import qualified BackendFile
import Data.Digest.Pure.SHA

-- based on BackendFile just with a different key type
backend = BackendFile.backend {
	name = "checksum",
	getKey = keyValue
}

-- checksum the file to get its key
keyValue :: GitRepo -> FilePath -> IO (Maybe Key)
keyValue k = error "checksum keyValue unimplemented" -- TODO
