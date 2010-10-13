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
 -
 - Escape "/" in the key name, to keep a flat tree of files and avoid
 - issues with keys containing "/../" or ending with "/" etc. 
 -
 - "/" is escaped to "%" because it's short and rarely used, and resembles
 -     a slash
 - "%" is escaped to "&s", and "&" to "&a"; this ensures that the mapping
 -     is one to one.
 - -}
keyFile :: Key -> FilePath
keyFile key = replace "/" "%" $ replace "%" "%s" $ replace "&" "&a" key

{- An annexed file's content is stored in 
 - .git/annex/<backend>/<key> ; this allows deriving the key and backend
 - by looking at the symlink to it. -}
annexLocation :: State -> Backend -> Key -> FilePath
annexLocation state backend key = 
	(gitWorkTree $ repo state) ++ "/" ++ 
	(annexLocationRelative state backend key)

{- Annexed file's location relative to the gitWorkTree -}
annexLocationRelative :: State -> Backend -> Key -> FilePath
annexLocationRelative state backend key = 
	gitDir (repo state) ++ "/annex/" ++ (name backend) ++ 
		"/" ++ (keyFile key)
