{- management of the git-annex branch
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Branch (
	create,
	update,
	get,
	change,
	commit,
	shortref
) where

import Control.Monad (unless, when, liftM)
import Control.Monad.State (liftIO)
import System.FilePath
import System.Directory
import Data.String.Utils
import System.Cmd.Utils
import Data.Maybe
import Data.List
import System.IO
import System.Posix.IO
import System.Posix.Process

import Types.BranchState
import qualified GitRepo as Git
import qualified GitUnionMerge
import qualified Annex
import Utility
import Types
import Messages

{- Name of the branch that is used to store git-annex's information. -}
name :: String
name = "git-annex"

{- Fully qualified name of the branch. -}
fullname :: String
fullname = "refs/heads/" ++ name

shortref :: String -> String
shortref = remove "refs/heads/" . remove "refs/remotes/"
	where
		remove prefix s
			| prefix `isPrefixOf` s = drop (length prefix) s
			| otherwise = s

{- A separate index file for the branch. -}
index :: Git.Repo -> FilePath
index g = Git.workTree g </> Git.gitDir g </> "index." ++ name

{- Populates the branch's index file with the current branch contents.
 - 
 - Usually, this is only done when the index doesn't yet exist, and
 - the index is used to build up changes to be commited to the branch.
 -}
genIndex :: Git.Repo -> IO ()
genIndex g = do
	ls <- Git.pipeNullSplit g $
		map Param ["ls-tree", "-z", "-r", "--full-tree", fullname]
	forceSuccess =<< Git.pipeWrite g
		(map Param ["update-index", "-z", "--index-info"])
		(join "\0" ls)

{- Runs an action using the branch's index file. -}
withIndex :: Annex a -> Annex a
withIndex a = do
	g <- Annex.gitRepo
	let f = index g
	liftIO $ Git.useIndex f

	e <- liftIO $ doesFileExist f
	unless e $ liftIO $ genIndex g

	r <- a
	liftIO $ Git.useDefaultIndex
	return r

withIndexUpdate :: Annex a -> Annex a
withIndexUpdate a = update >> withIndex a

getState :: Annex BranchState
getState = Annex.getState Annex.branchstate

setState :: BranchState -> Annex ()
setState state = Annex.changeState $ \s -> s { Annex.branchstate = state }

setCache :: FilePath -> String -> Annex ()
setCache file content = do
	state <- getState
	setState state { cachedFile = Just file, cachedContent = content }

setCacheChanged :: FilePath -> String -> Annex ()
setCacheChanged file content = do
	state <- getState
	setState state { cachedFile = Just file, cachedContent = content, branchChanged = True }

invalidateCache :: Annex ()
invalidateCache = do
	state <- getState
	setState state { cachedFile = Nothing, cachedContent = "" }

getCache :: FilePath -> Annex (Maybe String)
getCache file = getState >>= handle
	where
		handle state
			| cachedFile state == Just file =
				return $ Just $ cachedContent state
			| otherwise = return Nothing

{- Creates the branch, if it does not already exist. -}
create :: Annex ()
create = do
	exists <- refexists fullname
	unless exists $ do
		g <- Annex.gitRepo
		inorigin <- refexists origin
		if inorigin
			then liftIO $ Git.run g "branch" [Param name, Param origin]
			else liftIO $ do
				let f = index g
				liftIO $ Git.useIndex f
				GitUnionMerge.commit g "branch created" fullname []
				liftIO $ Git.useDefaultIndex
	where
		origin = "origin/" ++ name
		refexists ref = do
			g <- Annex.gitRepo
			liftIO $ Git.runBool g "show-ref"
				[Param "--verify", Param "-q", Param ref]

{- Commits any staged changes to the branch. -}
commit :: String -> Annex ()
commit message = do
	state <- getState
	when (branchChanged state) $ do
		g <- Annex.gitRepo
		withIndex $ liftIO $
			GitUnionMerge.commit g message fullname [fullname]

{- Ensures that the branch is up-to-date; should be called before
 - data is read from it. Runs only once per git-annex run. -}
update :: Annex ()
update = do
	state <- Annex.getState Annex.branchstate
	unless (branchUpdated state) $ withIndex $ do
		g <- Annex.gitRepo
		r <- liftIO $ Git.pipeRead g [Param "show-ref", Param name]
		let refs = map (last . words) (lines r)
		updated <- catMaybes `liftM` mapM updateRef refs
		unless (null updated) $ liftIO $
			GitUnionMerge.commit g "update" fullname
				(fullname:updated)
		Annex.changeState $ \s -> s { Annex.branchstate = state { branchUpdated = True } }
		invalidateCache

{- Ensures that a given ref has been merged into the index. -}
updateRef :: String -> Annex (Maybe String)
updateRef ref
	| ref == fullname = return Nothing
	| otherwise = do
		g <- Annex.gitRepo
		diffs <- liftIO $ Git.pipeRead g [
			Param "log",
			Param (name++".."++ref),
			Params "--oneline -n1"
			]
		if (null diffs)
			then return Nothing
			else do
				showSideAction $ "merging " ++ shortref ref ++ " into " ++ name ++ "..."
				-- By passing only one ref, it is actually
				-- merged into the index, preserving any
				-- changes that may already be staged.
				liftIO $ GitUnionMerge.merge g [ref]
				return $ Just ref

{- Stages the content of a file into the branch's index. -}
change :: FilePath -> String -> Annex ()
change file content = do
	g <- Annex.gitRepo
	sha <- liftIO $ Git.hashObject g content
	withIndex $ liftIO $ Git.run g "update-index"
		[ Param "--add", Param "--cacheinfo", Param "100644",
		  Param sha, File file]
	setCacheChanged file content

{- Gets the content of a file on the branch, or content staged in the index
 - if it's newer. Returns an empty string if the file didn't exist yet. -}
get :: FilePath -> Annex String
get file = do
	cached <- getCache file
	case cached of
		Just content -> return content
		Nothing -> withIndexUpdate $ do
			g <- Annex.gitRepo
			content <- liftIO $ catch (cat g) (const $ return "")
			setCache file content
			return content
	where
		cat g = cmdOutput "git" $ toCommand $ Git.gitCommandLine g
			[Param "cat-file", Param "blob", Param $ ':':file]

{- Runs a command, returning its output, ignoring nonzero exit
 - status, and discarding stderr. -}
cmdOutput :: FilePath -> [String] -> IO String
cmdOutput cmd params = do
	pipepair <- createPipe
	let callfunc _ = do
		closeFd (snd pipepair)
		h <- fdToHandle (fst pipepair)
		x <- hGetContentsStrict h
		hClose h
		return $! x
	pid <- pOpen3Raw Nothing (Just (snd pipepair)) Nothing cmd params
		(closeFd (fst pipepair) >> closeFd stdError)
	retval <- callfunc $! pid
	let rv = seq retval retval
	_ <- getProcessStatus True False pid
	return rv
