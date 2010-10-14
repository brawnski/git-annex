{- git-annex core functions -}

module Core where

import System.IO
import System.Directory
import Control.Monad.State (liftIO)
import Types
import BackendList
import Locations
import UUID
import qualified GitRepo as Git
import qualified Annex

{- Create and returns an Annex state object. 
 - Examines and prepares the git repo.
 -}
start :: IO AnnexState
start = do
	g <- Git.repoFromCwd
	let s = Annex.new g
	(_,s') <- Annex.run s (prep g)
	return s'
	where
		prep g = do
			-- setup git and read its config; update state
			g' <- liftIO $ Git.configRead g
			Annex.gitRepoChange g'
			liftIO $ gitSetup g'
			prepUUID

{- Sets up a git repo for git-annex. May be called repeatedly. -}
gitSetup :: Git.Repo -> IO ()
gitSetup repo = do
	-- configure git to use union merge driver on state files
	exists <- doesFileExist attributes
	if (not exists)
		then do
			writeFile attributes $ attrLine ++ "\n"
			commit
		else do
			content <- readFile attributes
			if (all (/= attrLine) (lines content))
				then do
					appendFile attributes $ attrLine ++ "\n"
					commit
				else return ()
	where
		attrLine = stateLoc ++ "/*.log merge=union"
		attributes = Git.attributes repo
		commit = do
			Git.run repo ["add", attributes]
			Git.run repo ["commit", "-m", "git-annex setup", 
					attributes]

{- Checks if a given key is currently present in the annexLocation -}
inAnnex :: Backend -> Key -> Annex Bool
inAnnex backend key = do
	g <- Annex.gitRepo
	liftIO $ doesFileExist $ annexLocation g backend key
