{- git-annex "checksum" backend
 - -}

module BackendChecksum (backend) where

import qualified BackendFile
import Data.Digest.Pure.SHA
import Types

-- based on BackendFile just with a different key type
backend = BackendFile.backend {
	name = "checksum",
	getKey = keyValue
}

-- checksum the file to get its key
keyValue :: State -> FilePath -> IO (Maybe Key)
keyValue k = error "checksum keyValue unimplemented" -- TODO
