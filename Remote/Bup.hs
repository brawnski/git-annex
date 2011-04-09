{- Using bup as a remote.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Bup (remote) where

import IO
import Control.Exception.Extensible (IOException)
import qualified Data.Map as M
import Control.Monad (unless)
import Control.Monad.State (liftIO)
import System.Process
import System.Exit

import RemoteClass
import Types
import qualified GitRepo as Git
import qualified Annex
import UUID
import Locations
import LocationLog
import Config
import Utility
import Messages
import Remote.Special

remote :: RemoteType Annex
remote = RemoteType {
	typename = "bup",
	enumerate = findSpecialRemotes "bupremote",
	generate = gen,
	setup = bupSetup
}

gen :: Git.Repo -> UUID -> Maybe (M.Map String String) -> Annex (Remote Annex)
gen r u c = do
	bupremote <- getConfig r "bupremote" (error "missing bupremote")
	let local = ':' `notElem` bupremote
	cst <- remoteCost r (if local then cheapRemoteCost else expensiveRemoteCost)
	
	return $ this cst bupremote
	where
		this cst bupremote = Remote {
			uuid = u,
			cost = cst,
			name = Git.repoDescribe r,
 			storeKey = store r bupremote,
			retrieveKeyFile = retrieve bupremote,
			removeKey = remove,
			hasKey = checkPresent u,
			hasKeyCheap = True,
			config = c
		}

bupSetup :: UUID -> M.Map String String -> Annex (M.Map String String)
bupSetup u c = do
	-- verify configuration is sane
	let bupremote = case M.lookup "remote" c of
		Nothing -> error "Specify remote="
		Just r -> r
	case M.lookup "encryption" c of
		Nothing -> error "Specify encryption=key or encryption=none"
		Just "none" -> return ()
		Just _ -> error "encryption keys not yet supported"

	-- bup init will create the repository.
	-- (If the repository already exists, bup init again appears safe.)
	showNote "bup init"
	ok <- bup "init" bupremote []
	unless ok $ error "bup init failed"

	-- The bup remote is stored in git config, as well as this remote's
	-- persistant state, so it can vary between hosts.
	gitConfigSpecialRemote u c "bupremote" bupremote

	return $ M.delete "directory" c

bupParams :: String -> String -> [CommandParam] -> [CommandParam]
bupParams command bupremote params = 
	(Param command) : [Param "-r", Param bupremote] ++ params

bup :: String -> String -> [CommandParam] -> Annex Bool
bup command bupremote params = do
	showProgress -- make way for bup output
	liftIO $ boolSystem "bup" $ bupParams command bupremote params

store :: Git.Repo -> String -> Key -> Annex Bool
store r bupremote k = do
	g <- Annex.gitRepo
	let src = gitAnnexLocation g k
	o <- getConfig r "bup-split-options" ""
	let os = map Param $ words o
	bup "split" bupremote $ os ++ [Param "-n", Param (show k), File src]

retrieve :: String -> Key -> FilePath -> Annex Bool
retrieve bupremote k f = do
	let params = bupParams "join" bupremote [Param $ show k]
	ret <- liftIO $ try $ do
		-- pipe bup's stdout directly to file
		tofile <- openFile f WriteMode
		p <- runProcess "bup" (toCommand params) 
			Nothing Nothing Nothing (Just tofile) Nothing
		r <- waitForProcess p
		case r of
			ExitSuccess -> return True
			_ -> return False
	case ret of
		Right r -> return r
		Left _ -> return False

remove :: Key -> Annex Bool
remove _ = do
	warning "content cannot be removed from bup remote"
	return False

{- Bup does not provide a way to tell if a given dataset is present
 - in a bup repository. One way it to check if the git repository has
 - a branch matching the name (as created by bup split -n).
 -
 - However, git-annex's ususal reasons for checking if a remote really
 - has a key also don't really apply in the case of bup, since, short
 - of deleting bup's git repository, data cannot be removed from it.
 - 
 - So, trust git-annex's location log; if it says a bup repository has
 - content, assume it's right.
 -}
checkPresent :: UUID -> Key -> Annex (Either IOException Bool)
checkPresent u k = do
	g <- Annex.gitRepo
	liftIO $ try $ do
		uuids <- keyLocations g k
		return $ u `elem` uuids
