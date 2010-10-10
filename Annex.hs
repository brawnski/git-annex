{- git-annex
 -}

module Annex where

import Backend
import System.Posix.Files
import System.Directory
import GitRepo
import Utility

{- An annexed file's content is stored in .git/annex/. -}
annexedFileLocation repo file = do
	dir <- gitDir repo
	return $ dir ++ "/annex/" ++ (gitRelative repo file)

{- Annexes a file, storing it in a backend, and then moving it into
 - the annex directory and setting up the symlink pointing to its
 - content. -}
annexFile :: [Backend] -> GitRepo -> FilePath -> IO ()
annexFile backends repo file = do
	alreadyannexed <- lookupBackend backends repo file
	case (alreadyannexed) of
		Just _ -> error $ "already annexed " ++ file
		Nothing -> do
			stored <- storeFile backends repo file
			if (not stored)
				then error $ "no backend could store " ++ file
				else symlink
	where
		symlink = do
			dest <- annexedFileLocation repo file
			createDirectoryIfMissing True (parentDir dest)
			renameFile file dest
			createSymbolicLink dest file
			gitAdd repo file
