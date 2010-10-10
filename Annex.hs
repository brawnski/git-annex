{- git-annex
 -}

module Annex where

import System.Posix.Files
import System.Directory
import GitRepo
import Utility
import Locations
import Backend

{- Annexes a file, storing it in a backend, and then moving it into
 - the annex directory and setting up the symlink pointing to its content. -}
annexFile :: [Backend] -> GitRepo -> FilePath -> IO ()
annexFile backends repo file = do
	alreadyannexed <- lookupBackend backends repo file
	case (alreadyannexed) of
		Just _ -> error $ "already annexed " ++ file
		Nothing -> do
			stored <- storeFile backends repo file
			case (stored) of
				Nothing -> error $ "no backend could store " ++ file
				Just key -> symlink key
	where
		symlink key = do
			dest <- annexDir repo key
			createDirectoryIfMissing True (parentDir dest)
			renameFile file dest
			createSymbolicLink dest file
			gitAdd repo file

{- Sets up a git repo for git-annex. May be called repeatedly. -}
gitPrep :: GitRepo -> IO ()
gitPrep repo = do
	-- configure git to use union merge driver on state files
	let attrLine = stateLoc ++ "/* merge=union"
	attributes <- gitAttributes repo
	exists <- doesFileExist attributes
	if (not exists)
		then do
			writeFile attributes $ attrLine ++ "\n"
			gitAdd repo attributes
		else do
			content <- readFile attributes
			if (all (/= attrLine) (lines content))
				then do
					appendFile attributes $ attrLine ++ "\n"
					gitAdd repo attributes
				else return ()

