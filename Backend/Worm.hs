{- git-annex "WORM" backend -- Write Once, Read Many
 - -}

module Backend.Worm (backend) where

import qualified Backend.File
import BackendTypes
import Utility
import System.FilePath

backend = Backend.File.backend {
	name = "WORM",
	getKey = keyValue
}

-- direct mapping from basename of filename to key
keyValue :: FilePath -> Annex (Maybe Key)
keyValue file = return $ Just $ Key ((name backend), (takeFileName file))
