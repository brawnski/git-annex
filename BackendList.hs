{- git-annex backend list
 - -}

module BackendList (allBackends) where

import BackendTypes

-- When adding a new backend, import it here and add it to the list.
import qualified Backend.WORM
import qualified Backend.SHA1
import qualified Backend.URL
allBackends = 
	[ Backend.WORM.backend
	, Backend.SHA1.backend
	, Backend.URL.backend
	]
