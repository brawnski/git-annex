{- git-annex file locations
 -}

module Locations (
	gitStateDir,
	stateLoc,
	keyFile,
	annexLocation,
	backendFile
) where

import Data.String.Utils
import Types
import GitRepo

{- Long-term, cross-repo state is stored in files inside the .git-annex
 - directory, in the git repository's working tree. -}
stateLoc = ".git-annex"
gitStateDir :: GitRepo -> FilePath
gitStateDir repo = (gitWorkTree repo) ++ "/" ++ stateLoc ++ "/"

{- Generates a filename that can be used to record a key somewhere to disk.
 - Just escape "/" in the key name, to keep a flat
 - tree of files and avoid issues with files ending with "/" etc. -}
keyFile :: Key -> FilePath
keyFile key = replace "/" "%" $ replace "%" "%%" key

{- An annexed file's content is stored somewhere under .git/annex/,
 - based on the key. -}
annexLocation :: State -> Key -> FilePath
annexLocation state key = gitDir (repo state) ++ "/annex/" ++ (keyFile key)

{- Name of state file that holds the key for an annexed file,
 - using a given backend. -}
backendFile :: State -> Backend -> FilePath -> String
backendFile state backend file =
	gitStateDir (repo state) ++ (gitRelative (repo state) file) ++ 
		"." ++ (name backend)
