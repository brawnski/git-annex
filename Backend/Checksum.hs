{- git-annex "checksum" backend
 - -}

module Backend.Checksum (backend) where

import qualified Backend.File
import Data.Digest.Pure.SHA
import BackendTypes

backend = Backend.File.backend {
	name = "checksum",
	getKey = keyValue
}

-- checksum the file to get its key
keyValue :: FilePath -> Annex (Maybe Key)
keyValue k = error "checksum keyValue unimplemented" -- TODO
