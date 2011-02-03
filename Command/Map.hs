{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Map where

import Control.Monad.State (liftIO)
import Control.Exception.Extensible
import System.Cmd.Utils

import Command
import qualified Annex
import qualified GitRepo as Git
import qualified Remotes
import Messages
import Types
import Utility

-- a link from the first repository to the second (its remote)
data Link = Link Git.Repo Git.Repo

command :: [Command]
command = [Command "map" paramNothing seek "generate map of repositories"]

seek :: [CommandSeek]
seek = [withNothing start]

start :: CommandStartNothing
start = do
	g <- Annex.gitRepo
	rs <- spider g

	liftIO $ writeFile file (dotGraph rs)
	showLongNote $ "running: dot -Tx11 " ++ file ++ "\n"
	r <- liftIO $ boolSystem "dot" ["-Tx11", file]
	return $ Just $ return $ Just $ return r
	where
		file = "map.dot"

{- Generates a graph for dot(1). Each repository is displayed
 - as a node, and each of its remotes is represented as an edge
 - pointing at the node for the remote. -}
dotGraph :: [Git.Repo] -> String
dotGraph rs = unlines $ [header] ++ map dotGraphRepo rs ++ [footer]
	where
		header = "digraph map {"
		footer= "}"

dotGraphRepo :: Git.Repo -> String
dotGraphRepo r = unlines $ map dotline (node:edges)
	where
		node = nodename r ++
			" [ label=" ++ dotquote (Git.repoDescribe r) ++ " ]"
		edges = map edge (Git.remotes r)
		edge e = nodename r ++ " -> " ++ nodename (makeabs r e)
		nodename n = dotquote (Git.repoLocation n)
		dotquote s = "\"" ++ s ++ "\""
		dotline s = "\t" ++ s ++ ";"

{- Recursively searches out remotes starting with the specified repo. -}
spider :: Git.Repo -> Annex [Git.Repo]
spider r = spider' [r] []
spider' :: [Git.Repo] -> [Git.Repo] -> Annex [Git.Repo]
spider' [] known = return known
spider' (r:rs) known
	| any (same r) known = spider' rs known
	| otherwise = do
		r' <- scan r
		let remotes = map (makeabs r') (Git.remotes r')
		spider' (rs ++ remotes) (r':known)

{- Makes a remote have an absolute url, rather than a host-local path. -}
makeabs :: Git.Repo -> Git.Repo -> Git.Repo
makeabs repo remote
	| Git.repoIsUrl remote = remote
	| not $ Git.repoIsUrl repo = remote
	| otherwise = Git.repoFromUrl combinedurl
		where
			combinedurl =
				Git.urlScheme repo ++ "//" ++
				Git.urlHost repo ++
				Git.workTree remote

{- Checks if two repos are the same. -}
same :: Git.Repo -> Git.Repo -> Bool
same a b
	| both Git.repoIsSsh = matching Git.urlHost && matching Git.workTree
	| both Git.repoIsUrl && neither Git.repoIsSsh = matching show
	| otherwise = False
		
	where
		matching t = t a == t b
		both t = t a && t b
		neither t = not (t a) && not (t b)

{- reads the config of a remote, with progress display -}
scan :: Git.Repo -> Annex Git.Repo
scan r = do
	showStart "map" (Git.repoDescribe r)
	v <- tryScan r
	case v of
		Just r' -> do
			showEndOk
			return r'
		Nothing -> do
			showEndFail
			return r

{- tries to read the config of a remote, returning it only if it can
 - be accessed -}
tryScan :: Git.Repo -> Annex (Maybe Git.Repo)
tryScan r
	| Git.repoIsSsh r = sshscan
	| Git.repoIsUrl r = return Nothing
	| otherwise = safely $ Git.configRead r
	where
		safely a = do
			result <- liftIO (try (a)::IO (Either SomeException Git.Repo))
			case result of
				Left _ -> return Nothing
				Right r' -> return $ Just r'
		pipedconfig cmd params = safely $
			pOpen ReadFromPipe cmd params $
				Git.hConfigRead r

		configlist =
			Remotes.onRemote r (pipedconfig, Nothing) "configlist" []
		manualconfiglist = do
			sshoptions <- Remotes.repoConfig r "ssh-options" ""
			let sshcmd =
				"cd " ++ shellEscape(Git.workTree r) ++ " && " ++
				"git config --list"
			liftIO $ pipedconfig "ssh" $
				words sshoptions ++ [Git.urlHost r, sshcmd]

		-- First, try sshing and running git config manually,
		-- only fall back to git-annex-shell configlist if that
		-- fails.
		-- 
		-- This is done for two reasons, first I'd like this
		-- subcommand to be usable on non-git-annex repos.
		-- Secondly, configlist doesn't include information about
		-- the remote's remotes.
		sshscan = do
			showNote "sshing..."
			showProgress
			v <- manualconfiglist
			case v of
				Nothing -> configlist
				ok -> return ok
