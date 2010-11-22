{- git-annex file locations
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Locations (
	gitStateDir,
	stateLoc,
	keyFile,
	fileKey,
	annexLocation,
	annexLocationRelative,
	annexTmpLocation,
	annexBadLocation,
	annexUnusedLog,
	annexDir,
	annexObjectDir,

	prop_idempotent_fileKey
) where

import Data.String.Utils

import Types
import qualified GitRepo as Git

{- Long-term, cross-repo state is stored in files inside the .git-annex
 - directory, in the git repository's working tree. -}
stateLoc :: String
stateLoc = ".git-annex/"
gitStateDir :: Git.Repo -> FilePath
gitStateDir repo = Git.workTree repo ++ "/" ++ stateLoc

{- Annexed file's absolute location. -}
annexLocation :: Git.Repo -> Key -> FilePath
annexLocation r key = 
	Git.workTree r ++ "/" ++ annexLocationRelative key

{- Annexed file's location relative to git's working tree. 
 -
 - Note: Assumes repo is NOT bare.-}
annexLocationRelative :: Key -> FilePath
annexLocationRelative key = ".git/annex/objects/" ++ f ++ "/" ++ f
	where
		f = keyFile key

{- The annex directory of a repository.
 -
 - Note: Assumes repo is NOT bare. -}
annexDir :: Git.Repo -> FilePath
annexDir r = Git.workTree r ++ "/.git/annex"

{- The part of the annex directory where file contents are stored.
 -}
annexObjectDir :: Git.Repo -> FilePath
annexObjectDir r = annexDir r ++ "/objects"

{- .git-annex/tmp/ is used for temp files -}
annexTmpLocation :: Git.Repo -> FilePath
annexTmpLocation r = annexDir r ++ "/tmp/"

{- .git-annex/bad/ is used for bad files found during fsck -}
annexBadLocation :: Git.Repo -> FilePath
annexBadLocation r = annexDir r ++ "/bad/"

{- .git/annex/unused is used to number possibly unused keys -}
annexUnusedLog :: Git.Repo -> FilePath
annexUnusedLog r = annexDir r ++ "/unused"

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
keyFile key = replace "/" "%" $ replace "%" "&s" $ replace "&" "&a"  $ show key

{- Reverses keyFile, converting a filename fragment (ie, the basename of
 - the symlink target) into a key. -}
fileKey :: FilePath -> Key
fileKey file = read $
	replace "&a" "&" $ replace "&s" "%" $ replace "%" "/" file

{- for quickcheck -}
prop_idempotent_fileKey :: String -> Bool
prop_idempotent_fileKey s = k == fileKey (keyFile k)
	where k = read $ "test:" ++ s
