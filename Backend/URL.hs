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

backend :: Backend Annex
backend = Backend {
	name = "URL",
	getKey = keyValue,
	storeFileKey = dummyStore,
	retrieveKeyFile = downloadUrl,
	-- allow keys to be removed; presumably they can always be
	-- downloaded again
	removeKey = dummyRemove,
	-- similarly, keys are always assumed to be out there on the web
	hasKey = dummyOk,
	-- and nothing needed to fsck
	fsckKey = dummyFsck
}

-- cannot generate url from filename
keyValue :: FilePath -> Annex (Maybe Key)
keyValue _ = return Nothing

-- cannot change url contents
dummyStore :: FilePath -> Key -> Annex Bool
dummyStore _ _ = return False

dummyRemove :: Key -> Maybe a -> Annex Bool
dummyRemove _ _ = return False

dummyFsck :: Key -> Maybe a -> Annex Bool
dummyFsck _ _ = return True

dummyOk :: Key -> Annex Bool
dummyOk _ = return True

downloadUrl :: Key -> FilePath -> Annex Bool
downloadUrl key file = do
	showNote "downloading"
	showProgress -- make way for curl progress bar
	liftIO $ boolSystem "curl" ["-#", "-o", file, url]
	where
		url = join ":" $ drop 1 $ split ":" $ show key 
