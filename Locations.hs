{- git-annex file locations
 -}

module Locations (
	gitStateDir,
	stateLoc,
	keyFile,
	fileKey,
	annexLocation,
	annexLocationRelative,
	annexTmpLocation
) where

import Data.String.Utils

import Types
import qualified TypeInternals as Internals
import qualified GitRepo as Git

{- Long-term, cross-repo state is stored in files inside the .git-annex
 - directory, in the git repository's working tree. -}
stateLoc = ".git-annex/"
gitStateDir :: Git.Repo -> FilePath
gitStateDir repo = (Git.workTree repo) ++ "/" ++ stateLoc

{- An annexed file's content is stored in 
 - /path/to/repo/.git/annex/<key>, where <key> is of the form
 - <backend:fragment>
 -
 - That allows deriving the key and backend by looking at the symlink to it.
 -}
annexLocation :: Git.Repo -> Key -> FilePath
annexLocation r key = 
	(Git.workTree r) ++ "/" ++ (annexLocationRelative r key)

{- Annexed file's location relative to git's working tree. 
 -
 - Note: Assumes repo is NOT bare.-}
annexLocationRelative :: Git.Repo -> Key -> FilePath
annexLocationRelative r key = ".git/annex/" ++ (keyFile key)

{- .git-annex/tmp is used for temp files
 -
 - Note: Assumes repo is NOT bare. -}
annexTmpLocation :: Git.Repo -> FilePath
annexTmpLocation r = (Git.workTree r) ++ ".git/annex/tmp/"

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
keyFile key = replace "/" "%" $ replace "%" "&s" $ replace "&" "&a" $ show key

{- Reverses keyFile, converting a filename fragment (ie, the basename of
 - the symlink target) into a key. -}
fileKey :: FilePath -> Key
fileKey file = read $
	replace "&a" "&" $ replace "&s" "%" $ replace "%" "/" file
