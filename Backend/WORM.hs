{- git-annex "WORM" backend -- Write Once, Read Many
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Backend.WORM (backends) where

import Control.Monad.State
import System.FilePath
import System.Posix.Files
import System.Directory
import Data.Maybe

import qualified Backend.File
import BackendTypes
import Locations
import qualified Annex
import Content
import Messages
import Types
import Key

backends :: [Backend Annex]
backends = [backend]

backend :: Backend Annex
backend = Backend.File.backend {
	name = "WORM",
	getKey = keyValue,
	fsckKey = Backend.File.checkKey checkKeySize
}

{- The key includes the file size, modification time, and the
 - basename of the filename.
 -
 - That allows multiple files with the same names to have different keys,
 - while also allowing a file to be moved around while retaining the
 - same key.
 -}
keyValue :: FilePath -> Annex (Maybe Key)
keyValue file = do
	stat <- liftIO $ getFileStatus file
	return $ Just $ Key {
		keyName = takeFileName file,
		keyBackendName = name backend,
		keySize = Just $ fromIntegral $ fileSize stat,
		keyMtime = Just $ modificationTime stat
	}

{- The size of the data for a key is checked against the size encoded in
 - the key's metadata. -}
checkKeySize :: Key -> Annex Bool
checkKeySize key = do
	g <- Annex.gitRepo
	let file = gitAnnexLocation g key
	present <- liftIO $ doesFileExist file
	if not present
		then return True
		else do
			s <- liftIO $ getFileStatus file
			if fromIntegral (fileSize s) == fromJust (keySize key)
				then return True
				else do
					dest <- moveBad key
					warning $ "Bad file size; moved to " ++ dest
					return False
