{- git-annex "URL" backend
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Backend.URL (backend) where

import Control.Exception
import Control.Monad.State (liftIO)
import Data.String.Utils
import System.Cmd
import System.Cmd.Utils
import System.Exit

import TypeInternals
import Core

backend = Backend {
	name = "URL",
	getKey = keyValue,
	storeFileKey = dummyStore,
	retrieveKeyFile = downloadUrl,
	removeKey = dummyOk,
	hasKey = dummyOk
}

-- cannot generate url from filename
keyValue :: FilePath -> Annex (Maybe Key)
keyValue file = return Nothing

-- cannot change url contents
dummyStore :: FilePath -> Key -> Annex Bool
dummyStore file url = return False

-- allow keys to be removed; presumably they can always be downloaded again
dummyOk :: Key -> Annex Bool
dummyOk url = return True

downloadUrl :: Key -> FilePath -> Annex Bool
downloadUrl key file = do
	showNote "downloading"
	liftIO $ putStrLn "" -- make way for curl progress bar
	result <- liftIO $ (try curl::IO (Either SomeException ()))
	case result of
		Left err -> return False
		Right succ -> return True
	where
		curl = safeSystem "curl" ["-#", "-o", file, url]
		url = join ":" $ drop 1 $ split ":" $ show key 
