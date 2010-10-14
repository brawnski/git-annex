{- git-annex "url" backend
 - -}

module Backend.Url (backend) where

import Control.Monad.State
import System.Cmd
import System.Exit
import BackendTypes

backend = Backend {
	name = "url",
	getKey = keyValue,
	storeFileKey = dummyStore,
	retrieveKeyFile = downloadUrl,
	removeKey = dummyRemove
}

-- cannot generate url from filename
keyValue :: FilePath -> Annex (Maybe Key)
keyValue file = return Nothing

-- cannot change url contents
dummyStore :: FilePath -> Key -> Annex Bool
dummyStore file url = return False
dummyRemove :: Key -> Annex Bool
dummyRemove url = return False

downloadUrl :: Key -> FilePath -> Annex Bool
downloadUrl url file = do
	liftIO $ putStrLn $ "download: " ++ (show url)
	result <- liftIO $ rawSystem "curl" ["-#", "-o", file, (show url)]
	if (result == ExitSuccess)
		then return True
		else return False
