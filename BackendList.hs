{- git-annex backend list
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module BackendList (allBackends) where

-- When adding a new backend, import it here and add it to the list.
import qualified Backend.WORM
import qualified Backend.SHA1
import qualified Backend.URL
allBackends = 
	[ Backend.WORM.backend
	, Backend.SHA1.backend
	, Backend.URL.backend
	]
