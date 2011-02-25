{- git-annex "SHA1" backend
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Backend.SHA1 (backend) where

import Control.Monad.State
import Data.String.Utils
import System.Cmd.Utils
import System.IO
import System.Directory

import qualified Backend.File
import BackendTypes
import Messages
import qualified Annex
import Locations
import Content
import Types
import Utility

backend :: Backend Annex
backend = Backend.File.backend {
	name = "SHA1",
	getKey = keyValue,
	fsckKey = Backend.File.checkKey checkKeySHA1
}

sha1 :: FilePath -> Annex String
sha1 file = do
	showNote "checksum..."
	liftIO $ pOpen ReadFromPipe "sha1sum" [utilityEscape file] $ \h -> do
		line <- hGetLine h
		let bits = split " " line
		if null bits
			then error "sha1sum parse error"
			else return $ head bits

-- A key is a sha1 of its contents.
keyValue :: FilePath -> Annex (Maybe Key)
keyValue file = do
	s <- sha1 file	
	return $ Just  $ Key (name backend, s)

-- A key's sha1 is checked during fsck.
checkKeySHA1 :: Key -> Annex Bool
checkKeySHA1 key = do
	g <- Annex.gitRepo
	let file = gitAnnexLocation g key
	present <- liftIO $ doesFileExist file
	if not present
		then return True
		else do
			s <- sha1 file
			if s == keyName key
				then return True
				else do
					dest <- moveBad key
					warning $ "Bad file content; moved to " ++ filePathToString dest
					return False
