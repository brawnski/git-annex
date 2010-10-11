{- git-annex file locations
 -}

module Locations where

import GitRepo

{- Long-term, cross-repo state is stored in files inside the .git-annex
 - directory, in the git repository. -}
stateLoc = ".git-annex"
gitStateDir :: GitRepo -> FilePath
gitStateDir repo = (top repo) ++ "/" ++ stateLoc ++ "/"
