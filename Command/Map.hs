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
import qualified Data.Map as M
import Data.List.Utils

import Command
import qualified Annex
import qualified GitRepo as Git
import qualified Remotes
import Messages
import Types
import Utility
import UUID

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

	umap <- uuidMap

	liftIO $ writeFile file (drawMap rs umap)
	showLongNote $ "running: dot -Tx11 " ++ file
	showProgress
	r <- liftIO $ boolSystem "dot" ["-Tx11", file]
	return $ Just $ return $ Just $ return r
	where
		file = "map.dot"

{- Generates a graph for dot(1). Each repository, and any other uuids, are
 - displayed as a node, and each of its remotes is represented as an edge
 - pointing at the node for the remote. -}
drawMap :: [Git.Repo] -> (M.Map UUID String) -> String
drawMap rs umap = dotGraph $ repos ++ others
	where
		repos = map (dotGraphRepo umap rs) rs
		others = map uuidnode (M.keys umap)
		uuidnode u = dotGraphNode u $ M.findWithDefault "" u umap

dotGraphRepo :: (M.Map UUID String) -> [Git.Repo] -> Git.Repo -> String
dotGraphRepo umap fullinfo r = unlines $ node:edges
	where
		node = inhost $ dotGraphNode (nodeid r) (repoName umap r)
		edges = map edge (Git.remotes r)

		inhost a
			| Git.repoIsUrl r = dotSubGraph hostname a
			| otherwise = a

		hostname = head $ split "." $ Git.urlHost r

		edge to =
			-- get the full info for the repo since its UUID
			-- is in there
			let to' = findfullinfo to
			in dotGraphEdge
				(nodeid r)
				(nodeid $ makeabs r to')
				(edgename to to')
		
		-- Only name an edge if the name is different than the name
		-- that will be used for the destination node. (This
		-- reduces visual clutter.)
		edgename to to' =
			case (Git.repoRemoteName to) of
				Nothing -> Nothing
				Just n ->
					if (n == repoName umap to')
						then Nothing
						else Just n

		nodeid n =
			case (getUncachedUUID n) of
				"" -> Git.repoLocation n
				u -> u
		findfullinfo n =
			case (filter (same n) fullinfo) of
				[] -> n
				(n':_) -> n'

repoName :: (M.Map UUID String) -> Git.Repo -> String
repoName umap r
	| null repouuid = fallback
	| otherwise = M.findWithDefault fallback repouuid umap
	where
		repouuid = getUncachedUUID r
		fallback =
			case (Git.repoRemoteName r) of
				Just n -> n
				Nothing -> "unknown"

dotGraphNode :: String -> String -> String
dotGraphNode nodeid desc = dotLineLabeled desc $ dotQuote nodeid

dotGraphEdge :: String -> String -> Maybe String -> String
dotGraphEdge fromid toid d =
	case d of
		Nothing -> dotLine edge
		Just desc -> dotLineLabeled desc edge
	where
		edge = dotQuote fromid ++ " -> " ++ dotQuote toid

dotGraph :: [String] -> String
dotGraph s = unlines $ [header] ++ s ++ [footer]
	where
		header = "digraph map {"
		footer= "}"

dotQuote :: String -> String
dotQuote s = "\"" ++ s ++ "\""

dotLine :: String -> String
dotLine s = "\t" ++ s ++ ";"

dotLineLabeled :: String -> String -> String
dotLineLabeled label s = dotLine $ s ++ " [ label=" ++ dotQuote label ++ " ]"

dotSubGraph :: String -> String -> String
dotSubGraph label s = "subgraph " ++ name ++ "{ " ++ setlabel ++ s ++ " }"
	where
		-- the "cluster_" makes dot draw a box
		name = dotQuote ("cluster_ " ++ label)
		setlabel = dotLine $ "label=" ++ dotQuote label

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
				Git.urlHostFull repo ++
				Git.workTree remote

{- Checks if two repos are the same. -}
same :: Git.Repo -> Git.Repo -> Bool
same a b
	| both Git.repoIsSsh = matching Git.urlHostFull && matching Git.workTree
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
				words sshoptions ++ [Git.urlHostFull r, sshcmd]

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
