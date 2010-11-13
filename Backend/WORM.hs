{- git-annex "WORM" backend -- Write Once, Read Many
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Backend.WORM (backend) where

import Control.Monad.State
import System.FilePath
import System.Posix.Files
import System.Posix.Types
import System.Directory
import Data.String.Utils

import qualified Backend.File
import TypeInternals
import Locations
import qualified Annex
import Core
import Messages

backend :: Backend
backend = Backend.File.backend {
	name = "WORM",
	getKey = keyValue,
	fsckKey = Backend.File.checkKey checkKeySize
}

-- The key is formed from the file size, modification time, and the
-- basename of the filename.
--
-- That allows multiple files with the same names to have different keys,
-- while also allowing a file to be moved around while retaining the
-- same key.
keyValue :: FilePath -> Annex (Maybe Key)
keyValue file = do
	stat <- liftIO $ getFileStatus file
	return $ Just $ Key ((name backend), key stat)
	where
 		key stat = uniqueid stat ++ sep ++ base
		uniqueid stat = (show $ modificationTime stat) ++ sep ++
			(show $ fileSize stat)
		base = takeFileName file
		sep = ":"

{- Extracts the file size from a key. -}
keySize :: Key -> FileOffset
keySize key = read $ section !! 2
	where
		section = split ":" (keyName key)

{- The size of the data for a key is checked against the size encoded in
 - the key. Note that the modification time is not checked. -}
checkKeySize :: Key -> Annex Bool
checkKeySize key = do
	g <- Annex.gitRepo
	let file = annexLocation g key
	present <- liftIO $ doesFileExist file
	if (not present)
		then return True
		else do
			s <- liftIO $ getFileStatus file
			if (fileSize s == keySize key)
				then return True
				else do
					dest <- moveBad key
					showNote $ "bad file size (moved to "++dest++")"
					return False
