{- git-annex "checksum" backend
 - -}

module Backend.Checksum (backend) where

import qualified Backend.File
import Data.Digest.Pure.SHA
import BackendTypes

-- based on BackendFile just with a different key type
backend = Backend.File.backend {
	name = "checksum",
	getKey = keyValue
}

-- checksum the file to get its key
keyValue :: FilePath -> Annex (Maybe Key)
keyValue k = error "checksum keyValue unimplemented" -- TODO
