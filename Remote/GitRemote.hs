{- Standard git remotes.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.GitRemote (generate) where

import Control.Exception.Extensible
import Control.Monad.State (liftIO)
import qualified Data.Map as Map
import Data.String.Utils
import System.Cmd.Utils
import Control.Monad (unless, filterM)

import RemoteClass
import Types
import qualified GitRepo as Git
import qualified Annex
import Locations
import UUID
import Utility
import qualified Content
import Messages
import CopyFile
import RsyncFile
import Ssh

generate :: Annex [Remote Annex]
generate = do
	readConfigs
	g <- Annex.gitRepo
	rs <- filterM repoNotIgnored (Git.remotes g)
	mapM genRemote rs
	
genRemote :: Git.Repo -> Annex (Remote Annex)
genRemote r = do
	u <- getUUID r
	c <- repoCost r
	return Remote {
		uuid = u,
		cost = c,
		name = Git.repoDescribe r,
		storeKey = copyToRemote r,
		retrieveKeyFile = copyFromRemote r,
		removeKey = error "TODO Remote.GitRemote.removeKey",
		hasKey = inAnnex r,
		hasKeyCheap = not (Git.repoIsUrl r)
	}

{- Reads the configs of git remotes.
 -
 - It's assumed to be cheap to read the config of non-URL remotes,
 - so this is done each time git-annex is run. Conversely,
 - the config of an URL remote is only read when there is no
 - cached UUID value.
 -}
readConfigs :: Annex ()
readConfigs = do
	g <- Annex.gitRepo
	allremotes <- filterM repoNotIgnored $ Git.remotes g
	let cheap = filter (not . Git.repoIsUrl) allremotes
	let expensive = filter Git.repoIsUrl allremotes
	doexpensive <- filterM cachedUUID expensive
	unless (null doexpensive) $
		showNote $ "getting UUID for " ++
			list doexpensive ++ "..."
	let todo = cheap ++ doexpensive
	unless (null todo) $ do
		mapM_ tryGitConfigRead todo
	where
		cachedUUID r = do
			u <- getUUID r
			return $ null u

{- The git configs for the git repo's remotes is not read on startup
 - because reading it may be expensive. This function tries to read the
 - config for a specified remote, and updates state. If successful, it
 - returns the updated git repo. -}
tryGitConfigRead :: Git.Repo -> Annex (Either Git.Repo Git.Repo)
tryGitConfigRead r 
	| not $ Map.null $ Git.configMap r = return $ Right r -- already read
	| Git.repoIsSsh r = store $ onRemote r (pipedconfig, r) "configlist" []
	| Git.repoIsUrl r = return $ Left r
	| otherwise = store $ safely $ Git.configRead r
	where
		-- Reading config can fail due to IO error or
		-- for other reasons; catch all possible exceptions.
		safely a = do
			result <- liftIO (try (a)::IO (Either SomeException Git.Repo))
			case result of
				Left _ -> return r
				Right r' -> return r'
		pipedconfig cmd params = safely $
			pOpen ReadFromPipe cmd (toCommand params) $
				Git.hConfigRead r
		store a = do
			r' <- a
			g <- Annex.gitRepo
			let l = Git.remotes g
			let g' = Git.remotesAdd g $ exchange l r'
			Annex.changeState $ \s -> s { Annex.repo = g' }
			return $ Right r'
		exchange [] _ = []
		exchange (old:ls) new =
			if Git.repoRemoteName old == Git.repoRemoteName new
				then new : exchange ls new
				else old : exchange ls new

{- Calculates cost for a repo.
 -
 - The default cost is 100 for local repositories, and 200 for remote
 - repositories; it can also be configured by remote.<name>.annex-cost
 -}
repoCost :: Git.Repo -> Annex Int
repoCost r = do
	c <- Annex.repoConfig r "cost" ""
	if not $ null c
		then return $ read c
		else if Git.repoIsUrl r
			then return 200
			else return 100

{- Checks if a repo should be ignored, based either on annex-ignore
 - setting, or on command-line options. Allows command-line to override
 - annex-ignore. -}
repoNotIgnored :: Git.Repo -> Annex Bool
repoNotIgnored r = do
	ignored <- Annex.repoConfig r "ignore" "false"
	to <- match Annex.toremote
	from <- match Annex.fromremote
	if to || from
		then return True
		else return $ not $ Git.configTrue ignored
	where
		match a = do
			n <- Annex.getState a
			return $ n == Git.repoRemoteName r

{- Checks if a given remote has the content for a key inAnnex.
 - If the remote cannot be accessed, returns a Left error.
 -}
inAnnex :: Git.Repo -> Key -> Annex (Either IOException Bool)
inAnnex r key = if Git.repoIsUrl r
		then checkremote
		else liftIO (try checklocal ::IO (Either IOException Bool))
	where
		checklocal = do
			-- run a local check inexpensively,
			-- by making an Annex monad using the remote
			a <- Annex.new r []
			Annex.eval a (Content.inAnnex key)
		checkremote = do
			showNote ("checking " ++ Git.repoDescribe r ++ "...")
			inannex <- onRemote r (boolSystem, False) "inannex" 
				[Param (show key)]
			return $ Right inannex

{- Tries to copy a key's content from a remote's annex to a file. -}
copyFromRemote :: Git.Repo -> Key -> FilePath -> Annex Bool
copyFromRemote r key file
	| not $ Git.repoIsUrl r = liftIO $ copyFile (gitAnnexLocation r key) file
	| Git.repoIsSsh r = rsynchelper r True key file
	| otherwise = error "copying from non-ssh repo not supported"

{- Tries to copy a key's content to a remote's annex. -}
copyToRemote :: Git.Repo -> Key -> Annex Bool
copyToRemote r key
	| not $ Git.repoIsUrl r = do
		g <- Annex.gitRepo
		let keysrc = gitAnnexLocation g key
		-- run copy from perspective of remote
		liftIO $ do
			a <- Annex.new r []
			Annex.eval a $ do
				ok <- Content.getViaTmp key $
					\f -> liftIO $ copyFile keysrc f
				Annex.queueRun
				return ok
	| Git.repoIsSsh r = do
		g <- Annex.gitRepo
		let keysrc = gitAnnexLocation g key
		rsynchelper r False key keysrc
	| otherwise = error "copying to non-ssh repo not supported"

rsynchelper :: Git.Repo -> Bool -> Key -> FilePath -> Annex (Bool)
rsynchelper r sending key file = do
	showProgress -- make way for progress bar
	p <- rsyncParams r sending key file
	res <- liftIO $ boolSystem "rsync" p
	if res
		then return res
		else do
			showLongNote "rsync failed -- run git annex again to resume file transfer"
			return res

{- Generates rsync parameters that ssh to the remote and asks it
 - to either receive or send the key's content. -}
rsyncParams :: Git.Repo -> Bool -> Key -> FilePath -> Annex [CommandParam]
rsyncParams r sending key file = do
	Just (shellcmd, shellparams) <- git_annex_shell r
		(if sending then "sendkey" else "recvkey")
		[ Param $ show key
		-- Command is terminated with "--", because
		-- rsync will tack on its own options afterwards,
		-- and they need to be ignored.
		, Param "--"
		]
	-- Convert the ssh command into rsync command line.
	let eparam = rsyncShell (Param shellcmd:shellparams)
	o <- Annex.repoConfig r "rsync-options" ""
	let base = options ++ map Param (words o) ++ eparam
	if sending
		then return $ base ++ [dummy, File file]
		else return $ base ++ [File file, dummy]
	where
		-- inplace makes rsync resume partial files
		options = [Params "-p --progress --inplace"]
		-- the rsync shell parameter controls where rsync
		-- goes, so the source/dest parameter can be a dummy value,
		-- that just enables remote rsync mode.
		dummy = Param ":"

{- Uses a supplied function to run a git-annex-shell command on a remote.
 -
 - Or, if the remote does not support running remote commands, returns
 - a specified error value. -}
onRemote 
	:: Git.Repo
	-> (FilePath -> [CommandParam] -> IO a, a)
	-> String
	-> [CommandParam]
	-> Annex a
onRemote r (with, errorval) command params = do
	s <- git_annex_shell r command params
	case s of
		Just (c, ps) -> liftIO $ with c ps
		Nothing -> return errorval

{- Generates parameters to run a git-annex-shell command on a remote. -}
git_annex_shell :: Git.Repo -> String -> [CommandParam] -> Annex (Maybe (FilePath, [CommandParam]))
git_annex_shell r command params
	| not $ Git.repoIsUrl r = return $ Just (shellcmd, shellopts)
	| Git.repoIsSsh r = do
		sshparams <- sshToRepo r [Param sshcmd]
		return $ Just ("ssh", sshparams)
	| otherwise = return Nothing
	where
		dir = Git.workTree r
		shellcmd = "git-annex-shell"
		shellopts = (Param command):(File dir):params
		sshcmd = shellcmd ++ " " ++ 
			unwords (map shellEscape $ toCommand shellopts)

{- Human visible list of remotes. -}
list :: [Git.Repo] -> String
list remotes = join ", " $ map Git.repoDescribe remotes
