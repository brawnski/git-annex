{- git-annex "URL" backend
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Backend.URL (backend) where

import Control.Monad.State (liftIO)
import Data.String.Utils

import TypeInternals
import Utility
import Messages

backend :: Backend
backend = Backend {
	name = "URL",
	getKey = keyValue,
	storeFileKey = dummyStore,
	retrieveKeyFile = downloadUrl,
	-- allow keys to be removed; presumably they can always be
	-- downloaded again
	removeKey = dummyOk,
	-- similarly, keys are always assumed to be out there on the web
	hasKey = dummyOk,
	-- and nothing needed to fsck
	fsckKey = dummyOk
}

-- cannot generate url from filename
keyValue :: FilePath -> Annex (Maybe Key)
keyValue _ = return Nothing

-- cannot change url contents
dummyStore :: FilePath -> Key -> Annex Bool
dummyStore _ _ = return False

dummyOk :: Key -> Annex Bool
dummyOk _ = return True

downloadUrl :: Key -> FilePath -> Annex Bool
downloadUrl key file = do
	showNote "downloading"
	showProgress -- make way for curl progress bar
	liftIO $ boolSystem "curl" ["-#", "-o", file, url]
	where
		url = join ":" $ drop 1 $ split ":" $ show key 
