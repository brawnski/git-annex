{- git-annex SHA abstract backend
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Backend.SHA (backends) where

import Control.Monad.State
import Data.String.Utils
import System.Cmd.Utils
import System.IO
import System.Directory
import Data.Maybe

import qualified Backend.File
import BackendTypes
import Messages
import qualified Annex
import Locations
import Content
import Types
import Utility
import qualified SysConfig

type SHASize = Int

backends :: [Backend Annex]
-- order is slightly significant; want sha1 first ,and more general
-- sizes earlier
backends = catMaybes $ map genBackend [1, 256, 512, 224, 384]

genBackend :: SHASize -> Maybe (Backend Annex)
genBackend size
	| supported size = Just b 
	| otherwise = Nothing
	where
		b = Backend.File.backend 
			{ name = shaName size
			, getKey = keyValue size
			, fsckKey = Backend.File.checkKey $ checkKeyChecksum size
			}
		supported 1 = SysConfig.sha1sum
		supported 256 = SysConfig.sha256sum
		supported 224 = SysConfig.sha224sum
		supported 384 = SysConfig.sha384sum
		supported 512 = SysConfig.sha512sum
		supported _ = False

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
