{- git-annex remote repositories
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remotes (
	list,
	tryGitConfigRead,
	readConfigs,
	keyPossibilities,
	inAnnex,
	same,
	byName,
	copyFromRemote,
	copyToRemote,
	onRemote
) where

import Control.Exception.Extensible
import Control.Monad.State (liftIO)
import qualified Data.Map as Map
import Data.String.Utils
import System.Cmd.Utils
import Data.List (intersect, sortBy)
import Control.Monad (when, unless, filterM)

import Types
import qualified GitRepo as Git
import qualified Annex
import LocationLog
import Locations
import UUID
import Trust
import Utility
import qualified Content
import Messages
import CopyFile
import RsyncFile
import Ssh

{- Human visible list of remotes. -}
list :: [Git.Repo] -> String
list remotes = join ", " $ map Git.repoDescribe remotes 

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

{- Reads the configs of all remotes.
 -
 - This has to be called before things that rely on eg, the UUID of
 - remotes. Most such things will take care of running this themselves.
 -
 - As reading the config of remotes can be expensive, this
 - function will only read configs once per git-annex run. It's
 - assumed to be cheap to read the config of non-URL remotes,
 - so this is done each time git-annex is run. Conversely,
 - the config of an URL remote is only read when there is no
 - cached UUID value.
 - -}
readConfigs :: Annex ()
readConfigs = do
	remotesread <- Annex.getState Annex.remotesread
	unless remotesread $ do
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
			Annex.changeState $ \s -> s { Annex.remotesread = True }
	where
		cachedUUID r = do
			u <- getUUID r
			return $ null u

{- Cost ordered lists of remotes that the LocationLog indicate may have a key.
 -
 - Also returns a list of UUIDs that are trusted to have the key
 - (some may not have configured remotes).
 -}
keyPossibilities :: Key -> Annex ([Git.Repo], [UUID])
keyPossibilities key = do
	readConfigs

	allremotes <- remotesByCost
	g <- Annex.gitRepo
	u <- getUUID g
	trusted <- trustGet Trusted

	-- get uuids of all repositories that are recorded to have the key
	uuids <- liftIO $ keyLocations g key
	let validuuids = filter (/= u) uuids

	-- note that validuuids is assumed to not have dups
	let validtrusteduuids = intersect validuuids trusted

	-- remotes that match uuids that have the key
	validremotes <- reposByUUID allremotes validuuids

	return (validremotes, validtrusteduuids)

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

{- Cost Ordered list of remotes. -}
remotesByCost :: Annex [Git.Repo]
remotesByCost = do
	g <- Annex.gitRepo
	reposByCost $ Git.remotes g

{- Orders a list of git repos by cost. Throws out ignored ones. -}
reposByCost :: [Git.Repo] -> Annex [Git.Repo]
reposByCost l = do
	notignored <- filterM repoNotIgnored l
	costpairs <- mapM costpair notignored
	return $ fst $ unzip $ sortBy cmpcost costpairs
	where
		costpair r = do
			cost <- repoCost r
			return (r, cost)
		cmpcost (_, c1) (_, c2) = compare c1 c2

{- Calculates cost for a repo.
 -
 - The default cost is 100 for local repositories, and 200 for remote
 - repositories; it can also be configured by remote.<name>.annex-cost
 -}
repoCost :: Git.Repo -> Annex Int
repoCost r = do
	cost <- Annex.repoConfig r "cost" ""
	if not $ null cost
		then return $ read cost
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
			name <- Annex.getState a
			case name of
				Nothing -> return False
				n -> return $ n == Git.repoRemoteName r

{- Checks if two repos are the same, by comparing their remote names. -}
same :: Git.Repo -> Git.Repo -> Bool
same a b = Git.repoRemoteName a == Git.repoRemoteName b

{- Looks up a remote by name. (Or by UUID.) -}
byName :: String -> Annex Git.Repo
byName "." = Annex.gitRepo -- special case to refer to current repository
byName name = do
	when (null name) $ error "no remote specified"
	g <- Annex.gitRepo
	match <- filterM matching $ Git.remotes g
	when (null match) $ error $
		"there is no git remote named \"" ++ name ++ "\""
	return $ head match
	where
		matching r = do
			if Just name == Git.repoRemoteName r
				then return True
				else do
					u <- getUUID r
					return $ (name == u)

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
