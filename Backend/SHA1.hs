{- git-annex "SHA1" backend
 - -}

module Backend.SHA1 (backend) where

import qualified Backend.File
import Data.Digest.Pure.SHA
import BackendTypes

backend = Backend.File.backend {
	name = "SHA1",
	getKey = keyValue
}

-- checksum the file to get its key
keyValue :: FilePath -> Annex (Maybe Key)
keyValue k = error "SHA1 keyValue unimplemented" -- TODO
