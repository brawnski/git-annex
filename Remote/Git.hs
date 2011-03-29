{- Standard git remotes.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Git (
	remote,
	onRemote
) where

import Control.Exception.Extensible
import Control.Monad.State (liftIO)
import qualified Data.Map as M
import System.Cmd.Utils
import Control.Monad (filterM, liftM)

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
import Config

remote :: RemoteType Annex
remote = RemoteType {
	typename = "git",
	generator = gen,
	setup = error "not supported"
}

gen :: Annex (RemoteGenerator Annex)
gen = do
	g <- Annex.gitRepo
	allremotes <- filterM remoteNotIgnored $ Git.remotes g

 	{- It's assumed to be cheap to read the config of non-URL remotes,
	 - so this is done each time git-annex is run. Conversely,
	 - the config of an URL remote is only read when there is no
	 - cached UUID value. -}
	let cheap = filter (not . Git.repoIsUrl) allremotes
	let expensive = filter Git.repoIsUrl allremotes
	expensive_todo <- filterM cachedUUID expensive
	let skip = filter (`notElem` expensive_todo) expensive
	let todo = cheap++expensive_todo

	let actions = map genRemote skip ++ 
		map (\r -> genRemote =<< tryGitConfigRead r) todo
	return (actions, map Git.repoDescribe expensive_todo)

	where
		cachedUUID r = liftM null $ getUUID r
	
genRemote :: Git.Repo -> Annex (Remote Annex)
genRemote r = do
	u <- getUUID r
	c <- remoteCost r
	return Remote {
		uuid = u,
		cost = c,
		name = Git.repoDescribe r,
		storeKey = copyToRemote r,
		retrieveKeyFile = copyFromRemote r,
		removeKey = dropKey r,
		hasKey = inAnnex r,
		hasKeyCheap = not (Git.repoIsUrl r),
		config = Nothing
	}

{- Tries to read the config for a specified remote, updates state, and
 - returns the updated repo. -}
tryGitConfigRead :: Git.Repo -> Annex Git.Repo
tryGitConfigRead r 
	| not $ M.null $ Git.configMap r = return r -- already read
	| Git.repoIsSsh r = store $ onRemote r (pipedconfig, r) "configlist" []
	| Git.repoIsUrl r = return r
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
			return r'
		exchange [] _ = []
		exchange (old:ls) new =
			if Git.repoRemoteName old == Git.repoRemoteName new
				then new : exchange ls new
				else old : exchange ls new

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
	
dropKey :: Git.Repo -> Key -> Annex Bool
dropKey r key = 
	onRemote r (boolSystem, False) "dropkey"
		[ Params "--quiet --force"
		, Param $ show key
		]

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
	o <- getConfig r "rsync-options" ""
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
