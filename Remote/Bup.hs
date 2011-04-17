{- Using bup as a remote.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Bup (remote) where

import qualified Data.ByteString.Lazy.Char8 as L
import IO
import Control.Exception.Extensible (IOException)
import qualified Data.Map as M
import Control.Monad (unless, when)
import Control.Monad.State (liftIO)
import System.Process
import System.Exit
import System.FilePath
import Data.List.Utils
import System.Cmd.Utils

import RemoteClass
import Types
import qualified GitRepo as Git
import qualified Annex
import UUID
import Locations
import Config
import Utility
import Messages
import Ssh
import Remote.Special
import Remote.Encryptable
import Crypto

type BupRepo = String

remote :: RemoteType Annex
remote = RemoteType {
	typename = "bup",
	enumerate = findSpecialRemotes "buprepo",
	generate = gen,
	setup = bupSetup
}

gen :: Git.Repo -> UUID -> Maybe RemoteConfig -> Annex (Remote Annex)
gen r u c = do
	buprepo <- getConfig r "buprepo" (error "missing buprepo")
	cst <- remoteCost r (if bupLocal buprepo then semiCheapRemoteCost else expensiveRemoteCost)
	bupr <- liftIO $ bup2GitRemote buprepo
	(u', bupr') <- getBupUUID bupr u
	
	return $ encryptableRemote c
		(storeEncrypted r buprepo)
		(retrieveEncrypted buprepo)
		Remote {
			uuid = u',
			cost = cst,
			name = Git.repoDescribe r,
 			storeKey = store r buprepo,
			retrieveKeyFile = retrieve buprepo,
			removeKey = remove,
			hasKey = checkPresent r bupr',
			hasKeyCheap = True,
			config = c
		}

bupSetup :: UUID -> RemoteConfig -> Annex RemoteConfig
bupSetup u c = do
	-- verify configuration is sane
	let buprepo = case M.lookup "buprepo" c of
		Nothing -> error "Specify buprepo="
		Just r -> r
	c' <- encryptionSetup c

	-- bup init will create the repository.
	-- (If the repository already exists, bup init again appears safe.)
	showNote "bup init"
	ok <- bup "init" buprepo []
	unless ok $ error "bup init failed"

	storeBupUUID u buprepo

	-- The buprepo is stored in git config, as well as this repo's
	-- persistant state, so it can vary between hosts.
	gitConfigSpecialRemote u c' "buprepo" buprepo

	return c'

bupParams :: String -> BupRepo -> [CommandParam] -> [CommandParam]
bupParams command buprepo params = 
	(Param command) : [Param "-r", Param buprepo] ++ params

bup :: String -> BupRepo -> [CommandParam] -> Annex Bool
bup command buprepo params = do
	showProgress -- make way for bup output
	liftIO $ boolSystem "bup" $ bupParams command buprepo params

pipeBup :: [CommandParam] -> Maybe Handle -> Maybe Handle -> IO Bool
pipeBup params inh outh = do
	p <- runProcess "bup" (toCommand params)
		Nothing Nothing inh outh Nothing
	ok <- waitForProcess p
	case ok of
		ExitSuccess -> return True
		_ -> return False

bupSplitParams :: Git.Repo -> BupRepo -> Key -> CommandParam -> Annex [CommandParam]
bupSplitParams r buprepo k src = do
	o <- getConfig r "bup-split-options" ""
	let os = map Param $ words o
	showProgress -- make way for bup output
	return $ bupParams "split" buprepo 
		(os ++ [Param "-n", Param (show k), src])

store :: Git.Repo -> BupRepo -> Key -> Annex Bool
store r buprepo k = do
	g <- Annex.gitRepo
	let src = gitAnnexLocation g k
	params <- bupSplitParams r buprepo k (File src)
	liftIO $ boolSystem "bup" params

storeEncrypted :: Git.Repo -> BupRepo -> (Cipher, Key) -> Key -> Annex Bool
storeEncrypted r buprepo (cipher, enck) k = do
	g <- Annex.gitRepo
	let src = gitAnnexLocation g k
	params <- bupSplitParams r buprepo enck (Param "-")
	liftIO $ catchBool $ do
		content <- L.readFile src
		withEncryptedHandle cipher content $ \h -> do
			pipeBup params (Just h) Nothing

retrieve :: BupRepo -> Key -> FilePath -> Annex Bool
retrieve buprepo k f = do
	let params = bupParams "join" buprepo [Param $ show k]
	liftIO $ catchBool $ do
		tofile <- openFile f WriteMode
		pipeBup params Nothing (Just tofile)

retrieveEncrypted :: BupRepo -> (Cipher, Key) -> FilePath -> Annex Bool
retrieveEncrypted buprepo (cipher, enck) f = do
	let params = bupParams "join" buprepo [Param $ show enck]
	liftIO $ catchBool $ do
		(pid, h) <- hPipeFrom "bup" $ toCommand params
		content <- L.hGetContents h
		withDecryptedContent cipher content $ L.writeFile f
		forceSuccess pid
		return True

remove :: Key -> Annex Bool
remove _ = do
	warning "content cannot be removed from bup remote"
	return False

{- Bup does not provide a way to tell if a given dataset is present
 - in a bup repository. One way it to check if the git repository has
 - a branch matching the name (as created by bup split -n).
 -}
checkPresent :: Git.Repo -> Git.Repo -> Key -> Annex (Either IOException Bool)
checkPresent r bupr k
	| Git.repoIsUrl bupr = do
		showNote ("checking " ++ Git.repoDescribe r ++ "...")
		ok <- onBupRemote bupr boolSystem "git" params
		return $ Right ok
	| otherwise = liftIO $ try $ boolSystem "git" $ Git.gitCommandLine bupr params
	where
		params = 
			[ Params "show-ref --quiet --verify"
			, Param $ "refs/heads/" ++ show k]

{- Store UUID in the annex.uuid setting of the bup repository. -}
storeBupUUID :: UUID -> BupRepo -> Annex ()
storeBupUUID u buprepo = do
	r <- liftIO $ bup2GitRemote buprepo
	if Git.repoIsUrl r
		then do
			showNote "storing uuid"
			ok <- onBupRemote r boolSystem "git"
				[Params $ "config annex.uuid " ++ u]
			unless ok $ do error "ssh failed"
		else liftIO $ do
			r' <- Git.configRead r
			let olduuid = Git.configGet r' "annex.uuid" ""
			when (olduuid == "") $
				Git.run r' "config" [Param "annex.uuid", Param u]

onBupRemote :: Git.Repo -> (FilePath -> [CommandParam] -> IO a) -> FilePath -> [CommandParam] -> Annex a
onBupRemote r a command params = do
	let dir = shellEscape (Git.workTree r)
	sshparams <- sshToRepo r [Param $
			"cd " ++ dir ++ " && " ++ (unwords $ command : toCommand params)]
	liftIO $ a "ssh" sshparams

{- Allow for bup repositories on removable media by checking
 - local bup repositories to see if they are available, and getting their
 - uuid (which may be different from the stored uuid for the bup remote).
 -
 - If a bup repository is not available, returns a dummy uuid of "".
 - This will cause checkPresent to indicate nothing from the bup remote
 - is known to be present.
 -
 - Also, returns a version of the repo with config read, if it is local.
 -}
getBupUUID :: Git.Repo -> UUID -> Annex (UUID, Git.Repo)
getBupUUID r u
	| Git.repoIsUrl r = return (u, r)
	| otherwise = liftIO $ do
		ret <- try $ Git.configRead r
		case ret of
			Right r' -> return (Git.configGet r' "annex.uuid" "", r')
			Left _ -> return ("", r)

{- Converts a bup remote path spec into a Git.Repo. There are some
 - differences in path representation between git and bup. -}
bup2GitRemote :: BupRepo -> IO Git.Repo
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

bupLocal :: BupRepo -> Bool
bupLocal = notElem ':'
