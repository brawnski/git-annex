{- management of the git-annex branch
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Branch (
	update,
	get,
	change,
	commit
) where

import Control.Monad (unless)
import Control.Monad.State (liftIO)
import System.FilePath
import System.Directory
import Data.String.Utils
import System.Cmd.Utils

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

{- A separate index file for the branch. -}
index :: Git.Repo -> FilePath
index g = Git.workTree g </> Git.gitDir g </> "index." ++ name

{- Populates the branch's index file with the current branch contents.
 - 
 - Usually, this is only done when the index doesn't yet exist, and
 - the index is used to build up changes to be commited to the branch.
 -}
genIndex :: FilePath -> Git.Repo -> IO ()
genIndex f g = do
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
	unless e $ liftIO $ genIndex f g

	r <- a
	liftIO $ Git.useDefaultIndex
	return r

{- Ensures that the branch is up-to-date; should be called before
 - data is read from it. Runs only once per git-annex run. -}
update :: Annex ()
update = do
	updated <- Annex.getState Annex.updated
	unless updated $ withIndex $ do
		g <- Annex.gitRepo
		refs <- liftIO $ Git.pipeRead g [Param "show-ref", Param name]
		mapM_ updateRef $ map (last . words) (lines refs)
		Annex.changeState $ \s -> s { Annex.updated = True }

{- Ensures that a given ref has been merged into the local git-annex branch. -}
updateRef :: String -> Annex ()
updateRef ref
	| ref == fullname = return ()
	| otherwise = do
		g <- Annex.gitRepo
		diffs <- liftIO $ Git.pipeRead g [
			Param "log",
			Param (name++".."++ref),
			Params "--oneline -n1"
			]
		unless (null diffs) $ do
			showSideAction $ "merging " ++ ref ++ " into " ++ name ++ "..."
			liftIO $ unionMerge g fullname ref fullname True

{- Stages the content of a file into the branch's index. -}
change :: FilePath -> String -> Annex ()
change file content = do
	g <- Annex.gitRepo
	sha <- liftIO $ Git.hashObject g content
	withIndex $ liftIO $ Git.run g "update-index"
		[ Params "--add --cacheinfo 100644 ",
		  Param sha, File file]

{- Commits staged changes to the branch. -}
commit :: String -> Annex ()
commit message = withIndex $ do
	g <- Annex.gitRepo
	liftIO $ GitUnionMerge.commit g message branch []

{- Gets the content of a file on the branch, or content staged in the index
 - if it's newer. Returns an empty string if the file didn't exist yet. -}
get :: FilePath -> Annex String
get file = update >> do
	withIndex $ do
		g <- Annex.gitRepo
		liftIO $ catch (cat g) (const $ return "")
	where
		-- To avoid stderr from cat-file when file does not exist,
		-- first run it with -e to check that it exists.
		cat g = do
			Git.run g "cat-file" [Param "-e", catfile]
			Git.pipeRead g [Param "cat-file", Param "blob", catfile]
		catfile = Param $ ':':file
