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

import qualified Backend.File
import TypeInternals

backend = Backend.File.backend {
	name = "SHA1",
	getKey = keyValue
}

-- checksum the file to get its key
keyValue :: FilePath -> Annex (Maybe Key)
keyValue file = liftIO $ pOpen ReadFromPipe "sha1sum" [file] $ \h -> do
		line <- hGetLine h
		let bits = split " " line
		if (null bits)
			then error "sha1sum parse error"
			else return $ Just $ Key ((name backend), bits !! 0)
