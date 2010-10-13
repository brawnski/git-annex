{- git-annex file locations
 -}

module Locations (
	gitStateDir,
	stateLoc,
	keyFile,
	annexLocation,
	annexLocationRelative
) where

import Data.String.Utils
import Types
import GitRepo

{- Long-term, cross-repo state is stored in files inside the .git-annex
 - directory, in the git repository's working tree. -}
stateLoc = ".git-annex"
gitStateDir :: GitRepo -> FilePath
gitStateDir repo = (gitWorkTree repo) ++ "/" ++ stateLoc ++ "/"

{- Converts a key into a filename fragment.
 - Just escape "/" in the key name, to keep a flat
 - tree of files and avoid issues with files ending with "/" etc. -}
keyFile :: Key -> FilePath
keyFile key = replace "/" "&s" $ replace "&" "&a" key

{- An annexed file's content is stored in 
 - .git/annex/<backend>/<key> ; this allows deriving the key and backend
 - by looking at the symlink to it. -}
annexLocation :: State -> Backend -> Key -> FilePath
annexLocation state backend key = 
	(gitWorkTree $ repo state) ++ "/" ++ 
	(annexLocationRelative state backend key)
annexLocationRelative :: State -> Backend -> Key -> FilePath
annexLocationRelative state backend key =
	gitDir (repo state) ++ "/annex/" ++ (name backend) ++ 
		"/" ++ (keyFile key)
