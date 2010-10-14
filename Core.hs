{- git-annex core functions -}

module Core where

import System.IO
import System.Directory
import Control.Monad.State (liftIO)
import Types
import Locations
import UUID
import qualified GitRepo as Git
import qualified Annex
			
{- Sets up a git repo for git-annex. May be called repeatedly. -}
gitSetup :: Annex ()
gitSetup = do
	g <- Annex.gitRepo
	liftIO $ setupattributes g
	prepUUID
	where
		-- configure git to use union merge driver on state files
		setupattributes repo = do
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
