{- A "remote" that is just a filesystem directory.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Directory (remote) where

import IO
import Control.Exception.Extensible (IOException)
import qualified Data.Map as M
import Control.Monad (when)
import Control.Monad.State (liftIO)
import System.Directory hiding (copyFile)
import System.FilePath

import RemoteClass
import Types
import qualified GitRepo as Git
import qualified Annex
import UUID
import Locations
import CopyFile
import Config
import Content
import Utility
import Remote.Special

remote :: RemoteType Annex
remote = RemoteType {
	typename = "directory",
	enumerate = findSpecialRemotes "directory",
	generate = gen,
	setup = directorySetup
}

gen :: Git.Repo -> UUID -> Maybe (M.Map String String) -> Annex (Remote Annex)
gen r u _ = do
	dir <- getConfig r "directory" (error "missing directory")
	cst <- remoteCost r cheapRemoteCost
	return $ Remote {
		uuid = u,
		cost = cst,
		name = Git.repoDescribe r,
 		storeKey = store dir,
		retrieveKeyFile = retrieve dir,
		removeKey = remove dir,
		hasKey = checkPresent dir,
		hasKeyCheap = True,
		config = Nothing
	}

directorySetup :: UUID -> M.Map String String -> Annex (M.Map String String)
directorySetup u c = do
	-- verify configuration is sane
	let dir = case M.lookup "directory" c of
		Nothing -> error "Specify directory="
		Just d -> d
	e <- liftIO $ doesDirectoryExist dir
	when (not e) $ error $ "Directory does not exist: " ++ dir

	-- The directory is stored in git config, not in this remote's
	-- persistant state, so it can vary between hosts.
	gitConfigSpecialRemote u c "directory" dir
	return $ M.delete "directory" c

dirKey :: FilePath -> Key -> FilePath
dirKey d k = d </> hashDirMixed k </> f </> f
	where
		f = keyFile k

store :: FilePath -> Key -> Annex Bool
store d k = do
	g <- Annex.gitRepo
	let src = gitAnnexLocation g k
	liftIO $ catch (copy src) (const $ return False)
	where
		dest = dirKey d k
		dir = parentDir dest
		copy src = do
			createDirectoryIfMissing True dir
			allowWrite dir
			ok <- copyFile src dest
			when ok $ do
				preventWrite dest
				preventWrite dir
			return ok

retrieve :: FilePath -> Key -> FilePath -> Annex Bool
retrieve d k f = liftIO $ copyFile (dirKey d k) f

remove :: FilePath -> Key -> Annex Bool
remove d k = liftIO $ catch del (const $ return False)
	where
		file = dirKey d k
		dir = parentDir file
		del = do
			allowWrite dir
			removeFile file
			removeDirectory dir
			return True

checkPresent :: FilePath -> Key -> Annex (Either IOException Bool)
checkPresent d k = liftIO $ try $ doesFileExist (dirKey d k)
