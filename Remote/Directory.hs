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
import Data.String.Utils
import Control.Monad (when)
import Control.Monad.State (liftIO)
import System.Directory (doesDirectoryExist, doesFileExist, removeFile)
import System.FilePath

import RemoteClass
import Types
import qualified GitRepo as Git
import qualified Annex
import UUID
import Config
import Utility
import Locations
import CopyFile

remote :: RemoteType Annex
remote = RemoteType {
	typename = "directory",
	enumerate = list,
	generate = gen,
	setup = dosetup
}

list :: Annex [Git.Repo]
list = do
	g <- Annex.gitRepo
	return $ findDirectoryRemotes g

findDirectoryRemotes :: Git.Repo -> [Git.Repo]
findDirectoryRemotes r = map construct remotepairs
	where
		remotepairs = M.toList $ filterremotes $ Git.configMap r
		filterremotes = M.filterWithKey (\k _ -> directoryremote k)
		construct (k,_) = Git.repoRemoteNameSet Git.repoFromUnknown k
		directoryremote k = startswith "remote." k && endswith ".annex-directory" k

gen :: Git.Repo -> Maybe (M.Map String String) -> Annex (Remote Annex)
gen r c = do
	u <- getUUID r
	cst <- remoteCost r
	return $ genRemote r u c cst
	where

genRemote :: Git.Repo -> UUID -> Maybe (M.Map String String) -> Int -> Remote Annex
genRemote r u c cst = this
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

dosetup :: UUID -> M.Map String String -> Annex (M.Map String String)
dosetup u c = do
	-- verify configuration is sane
	let dir = case M.lookup "directory" c of
		Nothing -> error "Specify directory="
		Just d -> d
	e <- liftIO $ doesDirectoryExist dir
	when (not e) $ error $ "Directory does not exist: " ++ dir

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
