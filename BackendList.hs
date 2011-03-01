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
import qualified Backend.SHA256
import qualified Backend.SHA512
import qualified Backend.SHA224
import qualified Backend.SHA384
import qualified Backend.URL
import Types

allBackends :: [Backend Annex]
allBackends = 
	[ Backend.WORM.backend
	, Backend.SHA1.backend
	, Backend.SHA256.backend
	, Backend.SHA512.backend
	, Backend.SHA224.backend
	, Backend.SHA384.backend
	, Backend.URL.backend
	]
