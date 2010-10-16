{- git-annex "WORM" backend -- Write Once, Read Many
 - -}

module Backend.WORM (backend) where

import Control.Monad.State
import System.FilePath
import System.Posix.Files
import Data.Digest.Pure.SHA -- slow, but we only checksum filenames
import qualified Data.ByteString.Lazy.Char8 as B

import qualified Backend.File
import BackendTypes
import Utility

backend = Backend.File.backend {
	name = "WORM",
	getKey = keyValue
}

-- A SHA1 of the basename of the filename, plus the file size and
-- modification time, is used as the unique part of the key. That
-- allows multiple files with the same names to have different keys,
-- while also allowing a file to be moved around while retaining the
-- same key.
--
-- The basename of the filename is also included in the key, so it's clear
-- what the original filename was when a user sees the value.
keyValue :: FilePath -> Annex (Maybe Key)
keyValue file = do
	stat <- liftIO $ getFileStatus file
	return $ Just $ Key ((name backend), key stat)
	where
 		key stat = (checksum $ uniqueid stat) ++ sep ++ base
		checksum s = show $ sha1 $ B.pack s
		uniqueid stat = (show $ fileSize stat) ++ sep ++
			(show $ modificationTime stat)
		base = takeFileName file
		sep = ":"
