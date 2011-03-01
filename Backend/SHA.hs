{- git-annex SHA abstract backend
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Backend.SHA (genBackend) where

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

type SHASize = Int

-- Constructor for Backends using a given SHASize.
genBackend :: SHASize -> Backend Annex
genBackend size = Backend.File.backend 
	{ name = shaName size
	, getKey = keyValue size
	, fsckKey = Backend.File.checkKey $ checkKeyChecksum size
	}

shaName :: SHASize -> String
shaName size = "SHA" ++ show size

shaN :: SHASize -> FilePath -> Annex String
shaN size file = do
	showNote "checksum..."
	liftIO $ pOpen ReadFromPipe command (toCommand [File file]) $ \h -> do
		line <- hGetLine h
		let bits = split " " line
		if null bits
			then error $ command ++ " parse error"
			else return $ head bits
	where
		command = "sha" ++ (show size) ++ "sum"

-- A key is a checksum of its contents.
keyValue :: SHASize -> FilePath -> Annex (Maybe Key)
keyValue size file = do
	s <- shaN size file	
	return $ Just $ Key (shaName size, s)

-- A key's checksum is checked during fsck.
checkKeyChecksum :: SHASize -> Key -> Annex Bool
checkKeyChecksum size key = do
	g <- Annex.gitRepo
	let file = gitAnnexLocation g key
	present <- liftIO $ doesFileExist file
	if not present
		then return True
		else do
			s <- shaN size file
			if s == keyName key
				then return True
				else do
					dest <- moveBad key
					warning $ "Bad file content; moved to " ++ filePathToString dest
					return False
