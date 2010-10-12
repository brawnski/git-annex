{- git-annex file locations
 -}

module Locations (
	gitStateDir,
	stateLoc
) where

import GitRepo

{- Long-term, cross-repo state is stored in files inside the .git-annex
 - directory, in the git repository's working tree. -}
stateLoc = ".git-annex"
gitStateDir :: GitRepo -> FilePath
gitStateDir repo = (gitWorkTree repo) ++ "/" ++ stateLoc ++ "/"
