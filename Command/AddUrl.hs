{- git-annex command
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.AddUrl where

import Control.Monad.State (liftIO, when)
import Network.URI
import Data.String.Utils
import System.Directory

import Command
import qualified Backend
import qualified Remote.Web
import qualified Command.Add
import Messages
import Content
import PresenceLog

command :: [Command]
command = [repoCommand "addurl" paramPath seek "add urls to annex"]

seek :: [CommandSeek]
seek = [withStrings start]

start :: CommandStartString
start s = do
	let u = parseURI s
	case u of
		Nothing -> error $ "bad url " ++ s
		Just url -> do
			file <- liftIO $ url2file url
			showStart "addurl" file
			next $ perform s file
			
perform :: String -> FilePath -> CommandPerform
perform url file = do
	[(_, backend)] <- Backend.chooseBackends [file]
	showNote $ "downloading " ++ url
	ok <- Remote.Web.download file [url]
	if ok
		then do
			stored <- Backend.storeFileKey file backend
			case stored of
				Nothing -> stop
				Just (key, _) -> do
					moveAnnex key file
					Remote.Web.setUrl key url InfoPresent
					next $ Command.Add.cleanup file key
		else stop

url2file :: URI -> IO FilePath
url2file url = do
	let parts = filter safe $ split "/" $ uriPath url
	if null parts
		then fallback
		else do
			let file = last parts
			e <- doesFileExist file
			if e then fallback else return file
	where
		fallback = do
			let file = replace "/" "_" $ show url
			e <- doesFileExist file
			when e $ error "already have this url"
			return file
		safe s
			| null s = False
			| s == "." = False
			| s == ".." = False
			| otherwise = True
