{- management of the git-annex branch
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Branch (
	update,
	change
) where

import Control.Monad (unless)
import Control.Monad.State (liftIO)

import GitUnionMerge
import GitRepo as Git
import qualified Annex
import Utility
import Types
import Messages

name :: String
name = "git-annex"

fullname :: String
fullname = "refs/heads/" ++ name

{- Ensures that the branch is up-to-date; should be called before
 - data is read from it. Runs only once per git-annex run. -}
update :: Annex ()
update = do
	updated <- Annex.getState Annex.updated
	unless updated $ do
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
			showSideAction "merging " ++ ref ++ " into " ++ name ++ "..."
			liftIO $ unionMerge g fullname ref fullname

{- Stages the content of a file to be committed to the branch. -}
change :: FilePath -> String -> Annex ()
change file content = do
	update

{- Commits staged changes to the branch. -}
