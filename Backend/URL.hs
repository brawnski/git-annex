{- git-annex "URL" backend
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Backend.URL (backends) where

import Control.Monad.State (liftIO)

import Types
import BackendClass
import Utility
import Messages
import Key

backends :: [Backend Annex]
backends = [backend]

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
	fsckKey = dummyFsck,
	-- and key upgrade not needed
	upgradableKey = \_ -> return False
}

-- cannot generate url from filename
keyValue :: FilePath -> Annex (Maybe Key)
keyValue _ = return Nothing

-- cannot change url contents
dummyStore :: FilePath -> Key -> Annex Bool
dummyStore _ _ = return False

dummyRemove :: Key -> Maybe a -> Annex Bool
dummyRemove _ _ = return True

dummyFsck :: Key -> Maybe FilePath -> Maybe a -> Annex Bool
dummyFsck _ _ _ = return True

dummyOk :: Key -> Annex Bool
dummyOk _ = return True

downloadUrl :: Key -> FilePath -> Annex Bool
downloadUrl key file = do
	showNote $ "downloading"
	showProgress -- make way for curl progress bar
	liftIO $ boolSystem "curl" [Params "-# -o", File file, File url]
	where
		url = keyName key
