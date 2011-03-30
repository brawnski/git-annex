{- A "remote" that is just a local directory.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Directory (remote) where

import IO
import Control.Exception.Extensible (IOException)
import qualified Data.Map as M
import Data.Maybe
import Control.Monad (when)
import Control.Monad.State (liftIO)
import System.Directory (doesDirectoryExist, doesFileExist, removeFile)
import System.FilePath

import RemoteClass
import Types
import qualified GitRepo as Git
import qualified Annex
import UUID
import Utility
import Locations
import CopyFile
import Remote.Special

remote :: RemoteType Annex
remote = RemoteType {
	typename = "directory",
	enumerate = findSpecialRemotes "directory",
	generate = gen,
	setup = directorySetup
}

gen :: Git.Repo -> UUID -> Cost -> Maybe (M.Map String String) -> Annex (Remote Annex)
gen r u cst c = return this
	where
		this = Remote {
			uuid = u,
			cost = cst,
			name = Git.repoDescribe r,
	 		storeKey = store this,
			retrieveKeyFile = retrieve this,
			removeKey = remove this,
			hasKey = checkPresent this,
			hasKeyCheap = True,
			config = c
		}

directorySetup :: UUID -> M.Map String String -> Annex (M.Map String String)
directorySetup u c = do
	-- verify configuration is sane
	let dir = case M.lookup "directory" c of
		Nothing -> error "Specify directory="
		Just d -> d
	e <- liftIO $ doesDirectoryExist dir
	when (not e) $ error $ "Directory does not exist: " ++ dir

	gitConfigSpecialRemote "directory" u c
	g <- Annex.gitRepo
	liftIO $ do
		Git.run g "config" [Param (configsetting "annex-directory"), Param "true"]
		Git.run g "config" [Param (configsetting "annex-uuid"), Param u]
	return c
	where
		remotename = fromJust (M.lookup "name" c)
		configsetting s = "remote." ++ remotename ++ "." ++ s

dirKey :: Remote Annex -> Key -> FilePath
dirKey r k = dir </> show k
	where
		dir = fromJust $ M.lookup "directory" $ fromJust $ config r

store :: Remote Annex -> Key -> Annex Bool
store r k = do
	g <- Annex.gitRepo
	liftIO $ copyFile (gitAnnexLocation g k) (dirKey r k)

retrieve :: Remote Annex -> Key -> FilePath -> Annex Bool
retrieve r k f = liftIO $ copyFile (dirKey r k) f

remove :: Remote Annex -> Key -> Annex Bool
remove r k = liftIO $ catch
	(removeFile (dirKey r k) >> return True)
	(const $ return False)

checkPresent :: Remote Annex -> Key -> Annex (Either IOException Bool)
checkPresent r k = liftIO $ try $ doesFileExist (dirKey r k)
