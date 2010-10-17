{- git-annex core functions -}

module Core where

import Maybe
import System.IO
import System.Directory
import Control.Monad.State (liftIO)
import System.Path

import Types
import Locations
import UUID
import qualified GitRepo as Git
import qualified Annex
import Utility
			
{- Sets up a git repo for git-annex. -}
startup :: [Flag] -> Annex ()
startup flags = do
	mapM (\f -> Annex.flagChange f True) flags
	g <- Annex.gitRepo
	liftIO $ gitAttributes g
	prepUUID

{- When git-annex is done, it runs this. -}
shutdown :: Annex ()
shutdown = do
	g <- Annex.gitRepo
	nocommit <- Annex.flagIsSet NoCommit
	needcommit <- Annex.flagIsSet NeedCommit
	if (needcommit && not nocommit)
		then liftIO $ Git.run g ["commit", "-q", "-m", 
			"git-annex log update", gitStateDir g]
		else return ()

{- configure git to use union merge driver on state files, if it is not
 - already -}
gitAttributes :: Git.Repo -> IO ()
gitAttributes repo = do
	exists <- doesFileExist attributes
	if (not exists)
		then do
			writeFile attributes $ attrLine ++ "\n"
			commit
		else do
			content <- readFile attributes
			if (all (/= attrLine) (lines content))
				then do
					appendFile attributes $ attrLine ++ "\n"
					commit
				else return ()
	where
		attrLine = stateLoc ++ "/*.log merge=union"
		attributes = Git.attributes repo
		commit = do
			Git.run repo ["add", attributes]
			Git.run repo ["commit", "-m", "git-annex setup", 
					attributes]

{- Checks if a given key is currently present in the annexLocation -}
inAnnex :: Key -> Annex Bool
inAnnex key = do
	g <- Annex.gitRepo
	liftIO $ doesFileExist $ annexLocation g key

{- Adds, optionally also commits a file to git.
 -
 - All changes to the git repository should go through this function.
 -
 - This is careful to not rely on the index. It may have staged changes,
 - so only use operations that avoid committing such changes.
 -}
gitAdd :: FilePath -> Maybe String -> Annex ()
gitAdd file commitmessage = do
	nocommit <- Annex.flagIsSet NoCommit
	if (nocommit)
		then Annex.flagChange NeedCommit True
		else do
			g <- Annex.gitRepo
			liftIO $ Git.run g ["add", file]
			if (isJust commitmessage)
				then liftIO $ Git.run g ["commit", "-m",
					(fromJust commitmessage), file]
				else return ()

{- Calculates the relative path to use to link a file to a key. -}
calcGitLink :: FilePath -> Key -> Annex FilePath
calcGitLink file key = do
	g <- Annex.gitRepo
	cwd <- liftIO $ getCurrentDirectory
	let absfile = case (absNormPath cwd file) of
		Just f -> f
		Nothing -> error $ "unable to normalize " ++ file
	return $ (relPathDirToDir (parentDir absfile) (Git.workTree g)) ++
		annexLocationRelative g key
