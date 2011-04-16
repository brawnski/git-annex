{- A "remote" that is just a filesystem directory.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Directory (remote) where

import qualified Data.ByteString.Lazy.Char8 as L
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
import Remote.Encrypted
import Crypto

remote :: RemoteType Annex
remote = RemoteType {
	typename = "directory",
	enumerate = findSpecialRemotes "directory",
	generate = gen,
	setup = directorySetup
}

gen :: Git.Repo -> UUID -> Maybe RemoteConfig -> Annex (Remote Annex)
gen r u c = do
	dir <- getConfig r "directory" (error "missing directory")
	cst <- remoteCost r cheapRemoteCost
	return $ Remote {
		uuid = u,
		cost = cst,
		name = Git.repoDescribe r,
 		storeKey = storeKeyEncrypted c $ store dir,
		retrieveKeyFile = retrieveKeyFileEncrypted c $ retrieve dir,
		removeKey = removeKeyEncrypted c $ remove dir,
		hasKey = hasKeyEncrypted c $ checkPresent dir,
		hasKeyCheap = True,
		config = Nothing
	}

directorySetup :: UUID -> RemoteConfig -> Annex RemoteConfig
directorySetup u c = do
	-- verify configuration is sane
	let dir = case M.lookup "directory" c of
		Nothing -> error "Specify directory="
		Just d -> d
	e <- liftIO $ doesDirectoryExist dir
	when (not e) $ error $ "Directory does not exist: " ++ dir
	c' <- encryptionSetup c

	-- The directory is stored in git config, not in this remote's
	-- persistant state, so it can vary between hosts.
	gitConfigSpecialRemote u c' "directory" dir
	return $ M.delete "directory" c'

dirKey :: FilePath -> Key -> FilePath
dirKey d k = d </> hashDirMixed k </> f </> f
	where
		f = keyFile k

store :: FilePath -> Key -> Maybe (Cipher, Key) -> Annex Bool
store d k c = do
	g <- Annex.gitRepo
	let src = gitAnnexLocation g k	
	liftIO $ catch (copy src) (const $ return False)
	where
		copy src = case c of
			Just (cipher, enckey) -> do
				content <- L.readFile src
				let dest = dirKey d enckey
				prep dest
				withEncryptedContent cipher content $ \s -> do
					L.writeFile dest s
					cleanup True dest
			_ -> do
				let dest = dirKey d k
				prep dest
				ok <- copyFile src dest
				cleanup ok dest
		prep dest = liftIO $ do
			let dir = parentDir dest
			createDirectoryIfMissing True dir
			allowWrite dir
		cleanup ok dest = do
			when ok $ do
				let dir = parentDir dest
				preventWrite dest
				preventWrite dir
			return ok

retrieve :: FilePath -> Key -> FilePath -> Maybe (Cipher, Key) -> Annex Bool
retrieve d k f Nothing = liftIO $ copyFile (dirKey d k) f
retrieve d k f (Just (cipher, enckey)) = 
	liftIO $ flip catch (const $ return False) $ do
		content <- L.readFile (dirKey d enckey)
		withDecryptedContent cipher content $ L.writeFile f
		return True

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
