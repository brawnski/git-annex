{- Web remotes.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Web (
	remote
) where

import Control.Monad.State (liftIO)
import Control.Exception
import System.FilePath
import Network.Curl.Easy
import Network.Curl.Opts
import Network.Curl.Types
import Network.Curl.Code

import Types
import Types.Remote
import qualified Git
import Messages
import Utility
import UUID
import Config
import PresenceLog

remote :: RemoteType Annex
remote = RemoteType {
	typename = "web",
	enumerate = list,
	generate = gen,
	setup = error "not supported"
}

-- There is only one web remote, and it always exists.
-- (If the web should cease to exist, remove this module and redistribute
-- a new release to the survivors by carrier pigeon.)
list :: Annex [Git.Repo]
list = return [Git.repoRemoteNameSet Git.repoFromUnknown "remote.web.dummy"]

-- Dummy uuid for the whole web. Do not alter.
webUUID :: UUID
webUUID = "00000000-0000-0000-0000-000000000001"

gen :: Git.Repo -> UUID -> Maybe RemoteConfig -> Annex (Remote Annex)
gen r _ _ = 
	return $ Remote {
		uuid = webUUID,
		cost = expensiveRemoteCost,
		name = Git.repoDescribe r,
		storeKey = upload,
		retrieveKeyFile = download,
		removeKey = remove,
		hasKey = check,
		hasKeyCheap = False,
		config = Nothing
	}

{- The urls for a key are stored in remote/web/key.log in the git-annex branch. -}
urlLog :: Key -> FilePath
urlLog key = "remote/web" </> show key ++ ".log"

urls :: Key -> Annex [URLString]
urls key = currentLog (urlLog key)

download :: Key -> FilePath -> Annex Bool
download key file = download' file =<< urls key
download' :: FilePath -> [URLString] -> Annex Bool
download' _ [] = return False
download' file (url:us) = do
	showProgress -- make way for curl progress bar
	ok <- liftIO $ boolSystem "curl" [Params "-# -o", File file, File url]
	if ok then return ok else download' file us

upload :: Key -> Annex Bool
upload _ = do
	warning "upload to web not supported"
	return False

remove :: Key -> Annex Bool
remove _ = do
	warning "removal from web not supported"
	return False

check :: Key -> Annex (Either IOException Bool)
check key = do
	us <- urls key
	if null us
		then return $ Right False
		else return . Right =<< check' us
check' :: [URLString] -> Annex Bool
check' [] = return False
check' (u:us) = do
	showNote ("checking " ++ u)
	e <- liftIO $ urlexists u
	if e then return e else check' us

urlexists :: URLString -> IO Bool
urlexists url = do
	curl <- initialize
	_ <- setopt curl (CurlURL url)
	_ <- setopt curl (CurlNoBody True)
	_ <- setopt curl (CurlFailOnError True)
	res <- perform curl
	return $ res == CurlOK
