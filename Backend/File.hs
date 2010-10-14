{- git-annex "file" backend
 - -}

module Backend.File (backend) where

import Control.Monad.State
import System.IO
import System.Cmd
import System.Exit
import Control.Exception
import BackendTypes
import LocationLog
import Locations
import qualified Remotes
import qualified GitRepo as Git
import Utility
import Core

backend = Backend {
	name = "file",
	getKey = keyValue,
	storeFileKey = dummyStore,
	retrieveKeyFile = copyKeyFile,
	removeKey = dummyRemove,
	hasKey = checkKeyFile
}

-- direct mapping from filename to key
keyValue :: FilePath -> Annex (Maybe Key)
keyValue file = return $ Just $ Key file

{- This backend does not really do any independant data storage,
 - it relies on the file contents in .git/annex/ in this repo,
 - and other accessible repos. So storing a key is
 - a no-op. -}
dummyStore :: FilePath -> Key -> Annex (Bool)
dummyStore file key = return True

{- Allow keys to be removed. -}
dummyRemove :: Key -> Annex Bool
dummyRemove url = return True

{- Just check if the .git/annex/ file for the key exists. -}
checkKeyFile :: Key -> Annex Bool
checkKeyFile k = inAnnex backend k

{- Try to find a copy of the file in one of the remotes,
 - and copy it over to this one. -}
copyKeyFile :: Key -> FilePath -> Annex (Bool)
copyKeyFile key file = do
	remotes <- Remotes.withKey key
	trycopy remotes remotes
	where
		trycopy full [] = error $ "unable to get: " ++ (keyFile key) ++ "\n" ++
			"To get that file, need access to one of these remotes: " ++
			(Remotes.list full)
		trycopy full (r:rs) = do
			-- annexLocation needs the git config to have been
			-- read for a remote, so do that now,
			-- if it hasn't been already
			result <- Remotes.tryGitConfigRead r
			case (result) of
				Nothing -> trycopy full rs
				Just r' -> do
					result <- liftIO $ (try (copyFromRemote r' key file)::IO (Either SomeException ()))
		        		case (result) of
				                Left err -> do
							liftIO $ hPutStrLn stderr (show err)
							trycopy full rs
				                Right succ -> return True

{- Tries to copy a file from a remote, exception on error. -}
copyFromRemote :: Git.Repo -> Key -> FilePath -> IO ()
copyFromRemote r key file = do
	putStrLn $ "copy from " ++ (Git.repoDescribe r ) ++ " " ++ file

	if (Git.repoIsLocal r)
		then getlocal
		else getremote
	where
		getlocal = do
			res <-rawSystem "cp" ["-a", location, file]
			if (res == ExitSuccess)
				then return ()
				else error "cp failed"
		getremote = error "get via network not yet implemented!"
		location = annexLocation r backend key
