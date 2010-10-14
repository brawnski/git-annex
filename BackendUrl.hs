{- git-annex "url" backend
 - -}

module BackendUrl (backend) where

import Control.Monad.State
import System.Cmd
import IO
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
	result <- liftIO $ try $ rawSystem "curl" ["-#", "-o", file, (show url)]
	case (result) of
		Left _ -> return False
		Right _ -> return True
