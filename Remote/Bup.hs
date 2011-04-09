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
import Control.Monad (unless, when)
import Control.Monad.State (liftIO)
import System.Process
import System.Exit
import System.FilePath
import Data.List.Utils

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
import Ssh

remote :: RemoteType Annex
remote = RemoteType {
	typename = "bup",
	enumerate = findSpecialRemotes "buprepo",
	generate = gen,
	setup = bupSetup
}

gen :: Git.Repo -> UUID -> Maybe (M.Map String String) -> Annex (Remote Annex)
gen r u c = do
	buprepo <- getConfig r "buprepo" (error "missing buprepo")
	cst <- remoteCost r (if bupLocal buprepo then semiCheapRemoteCost else expensiveRemoteCost)
	u' <- getBupUUID buprepo u
	
	return $ this cst buprepo u'
	where
		this cst buprepo u' = Remote {
			uuid = u',
			cost = cst,
			name = Git.repoDescribe r,
 			storeKey = store r buprepo,
			retrieveKeyFile = retrieve buprepo,
			removeKey = remove,
			hasKey = checkPresent u',
			hasKeyCheap = True,
			config = c
		}

bupSetup :: UUID -> M.Map String String -> Annex (M.Map String String)
bupSetup u c = do
	-- verify configuration is sane
	let buprepo = case M.lookup "buprepo" c of
		Nothing -> error "Specify buprepo="
		Just r -> r
	case M.lookup "encryption" c of
		Nothing -> error "Specify encryption=key or encryption=none"
		Just "none" -> return ()
		Just _ -> error "encryption keys not yet supported"

	-- bup init will create the repository.
	-- (If the repository already exists, bup init again appears safe.)
	showNote "bup init"
	ok <- bup "init" buprepo []
	unless ok $ error "bup init failed"

	storeBupUUID u buprepo

	-- The buprepo is stored in git config, as well as this repo's
	-- persistant state, so it can vary between hosts.
	gitConfigSpecialRemote u c "buprepo" buprepo

	return $ M.delete "directory" c

bupParams :: String -> String -> [CommandParam] -> [CommandParam]
bupParams command buprepo params = 
	(Param command) : [Param "-r", Param buprepo] ++ params

bup :: String -> String -> [CommandParam] -> Annex Bool
bup command buprepo params = do
	showProgress -- make way for bup output
	liftIO $ boolSystem "bup" $ bupParams command buprepo params

store :: Git.Repo -> String -> Key -> Annex Bool
store r buprepo k = do
	g <- Annex.gitRepo
	let src = gitAnnexLocation g k
	o <- getConfig r "bup-split-options" ""
	let os = map Param $ words o
	bup "split" buprepo $ os ++ [Param "-n", Param (show k), File src]

retrieve :: String -> Key -> FilePath -> Annex Bool
retrieve buprepo k f = do
	let params = bupParams "join" buprepo [Param $ show k]
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

{- Store UUID in the annex.uuid setting of the bup repository. -}
storeBupUUID :: UUID -> FilePath -> Annex ()
storeBupUUID u buprepo = do
	r <- liftIO $ bup2GitRemote buprepo
	if Git.repoIsUrl r
		then do
			showNote "storing uuid"
			let dir = shellEscape (Git.workTree r)
			sshparams <- sshToRepo r
				[Param $ "cd " ++ dir ++
				 " && git config annex.uuid " ++ u]
			ok <- liftIO $ boolSystem "ssh" sshparams
			unless ok $ do error "ssh failed"
		else liftIO $ do
			r' <- Git.configRead r
			let olduuid = Git.configGet r' "annex.uuid" ""
			when (olduuid == "") $
				Git.run r' "config" [Param "annex.uuid", Param u]

{- Allow for bup repositories on removable media by checking
 - local bup repositories  -}
getBupUUID :: FilePath -> UUID -> Annex UUID
getBupUUID buprepo u = do
	return u -- TODO

{- Converts a bup remote path spec into a Git.Repo. There are some
 - differences in path representation between git and bup. -}
bup2GitRemote :: FilePath -> IO Git.Repo
bup2GitRemote "" = do
	-- bup -r "" operates on ~/.bup
	h <- myHomeDir
	Git.repoFromAbsPath $ h </> ".bup"
bup2GitRemote r
	| bupLocal r = 
		if r !! 0 == '/'
			then Git.repoFromAbsPath r
			else error "please specify an absolute path"
	| otherwise = Git.repoFromUrl $ "ssh://" ++ host ++ slash dir
		where
			bits = split ":" r
			host = bits !! 0
			dir = join ":" $ drop 1 bits
			-- "host:~user/dir" is not supported specially by bup;
			-- "host:dir" is relative to the home directory;
			-- "host:" goes in ~/.bup
			slash d
				| d == "" = "/~/.bup"
				| d !! 0 == '/' = d
				| otherwise = "/~/" ++ d

bupLocal :: FilePath -> Bool
bupLocal = notElem ':'
