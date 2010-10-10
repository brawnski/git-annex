{- git-annex backend list
 - -}

module BackendList where

-- When adding a new backend, import it here and add it to the list.
import qualified BackendFile
import qualified BackendChecksum
import qualified BackendUrl
supportedBackends = 
	[ BackendFile.backend
	, BackendChecksum.backend
	, BackendUrl.backend
	]
