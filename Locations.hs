{- git-annex file locations
 -}

module Locations (
	gitStateDir,
	stateLoc,
	keyFile,
	fileKey,
	annexLocation,
	annexLocationRelative
) where

import Data.String.Utils
import AbstractTypes
import qualified BackendTypes as Backend
import qualified GitRepo as Git

{- Long-term, cross-repo state is stored in files inside the .git-annex
 - directory, in the git repository's working tree. -}
stateLoc = ".git-annex"
gitStateDir :: Git.Repo -> FilePath
gitStateDir repo = (Git.workTree repo) ++ "/" ++ stateLoc ++ "/"

{- An annexed file's content is stored in 
 - /path/to/repo/.git/annex/<backend>/<key>
 -
 - (That allows deriving the key and backend by looking at the symlink to it.)
 -}
annexLocation :: Git.Repo -> Backend -> Key -> FilePath
annexLocation r backend key = 
	(Git.workTree r) ++ "/" ++ (annexLocationRelative r backend key)

{- Annexed file's location relative to the gitWorkTree -}
annexLocationRelative :: Git.Repo -> Backend -> Key -> FilePath
annexLocationRelative r backend key = 
	Git.dir r ++ "/annex/" ++ (Backend.name backend) ++ "/" ++ (keyFile key)

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
fileKey file = Backend.Key $ 
	replace "&a" "&" $ replace "&s" "%" $ replace "%" "/" file
