{- git-annex file locations
 -}

module Locations where

import Types
import GitRepo

{- An annexed file's content is stored somewhere under .git/annex/ -}
annexDir :: GitRepo -> Key -> IO FilePath
annexDir repo key = do
	dir <- gitDir repo
	return $ dir ++ "/annex/" ++ key

{- Long-term state is stored in files inside the .git-annex directory
 - in the git repository. -}
stateLoc = ".git-annex"
gitStateDir :: GitRepo -> FilePath
gitStateDir repo = (top repo) ++ "/" ++ stateLoc ++ "/"
