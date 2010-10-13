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
import Types
import GitRepo

{- Long-term, cross-repo state is stored in files inside the .git-annex
 - directory, in the git repository's working tree. -}
stateLoc = ".git-annex"
gitStateDir :: GitRepo -> FilePath
gitStateDir repo = (gitWorkTree repo) ++ "/" ++ stateLoc ++ "/"

{- An annexed file's content is stored in 
 - /path/to/repo/.git/annex/<backend>/<key>
 -
 - (That allows deriving the key and backend by looking at the symlink to it.)
 -}
annexLocation :: GitRepo -> Backend -> Key -> FilePath
annexLocation r backend key = 
	(gitWorkTree r) ++ "/" ++ (annexLocationRelative r backend key)

{- Annexed file's location relative to the gitWorkTree -}
annexLocationRelative :: GitRepo -> Backend -> Key -> FilePath
annexLocationRelative r backend key = 
	gitDir r ++ "/annex/" ++ (name backend) ++ "/" ++ (keyFile key)

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

{- Reverses keyFile -}
fileKey :: FilePath -> Key
fileKey file = Key $ replace "&a" "&" $ replace "&s" "%" $ replace "%" "/" file
