{- git-annex core functions -}

module Core where

import Maybe
import System.IO
import System.Directory
import Control.Monad.State (liftIO)
import System.Path
import Data.String.Utils

import Types
import Locations
import LocationLog
import UUID
import qualified GitRepo as Git
import qualified Annex
import Utility
			
{- Sets up a git repo for git-annex. -}
startup :: Annex Bool
startup = do
	g <- Annex.gitRepo
	liftIO $ gitAttributes g
	prepUUID
	return True

{- When git-annex is done, it runs this. -}
shutdown :: Annex Bool
shutdown = do
	g <- Annex.gitRepo

	liftIO $ Git.run g ["add", gitStateDir g]

	-- clean up any files left in the temp directory, but leave
	-- the tmp directory itself
	let tmp = annexTmpLocation g
	exists <- liftIO $ doesDirectoryExist tmp
	if (exists)
		then liftIO $ removeDirectoryRecursive $ tmp
		else return ()
	liftIO $ createDirectoryIfMissing True tmp

	return True

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
		attrLine = stateLoc ++ "*.log merge=union"
		attributes = Git.attributes repo
		commit = do
			Git.run repo ["add", attributes]
			Git.run repo ["commit", "-m", "git-annex setup", 
					attributes]

{- Checks if a given key is currently present in the annexLocation.
 -
 - This can be run against a remote repository to check the key there. -}
inAnnex :: Key -> Annex Bool
inAnnex key = do
	g <- Annex.gitRepo
	if (not $ Git.repoIsUrl g)
		then liftIO $ doesFileExist $ annexLocation g key
		else do
			showNote ("checking " ++ Git.repoDescribe g ++ "...")
			liftIO $ boolSystem "ssh" [Git.urlHost g,
				"test -e " ++
				(shellEscape $ annexLocation g key)]

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

{- Updates the LocationLog when a key's presence changes. -}
logStatus :: Key -> LogStatus -> Annex ()
logStatus key status = do
	g <- Annex.gitRepo
	u <- getUUID g
	liftIO $ logChange g key u status

{- Runs an action, passing it a temporary filename to download,
 - and if the action succeeds, moves the temp file into 
 - the annex as a key's content. -}
getViaTmp :: Key -> (FilePath -> Annex Bool) -> Annex Bool
getViaTmp key action = do
	g <- Annex.gitRepo
	let dest = annexLocation g key
	let tmp = (annexTmpLocation g) ++ (keyFile key)
	liftIO $ createDirectoryIfMissing True (parentDir tmp)
	success <- action tmp
	if (success)
		then do
			liftIO $ renameFile tmp dest
			logStatus key ValuePresent
			return True
		else do
			-- the tmp file is left behind, in case caller wants
			-- to resume its transfer
			return False

{- Output logging -}
verbose :: Annex () -> Annex ()
verbose a = do
	q <- Annex.flagIsSet "quiet"
	if (q) then return () else a
showStart :: String -> String -> Annex ()
showStart command file = verbose $ do
	liftIO $ putStr $ command ++ " " ++ file ++ " "
	liftIO $ hFlush stdout
showNote :: String -> Annex ()
showNote s = verbose $ do
	liftIO $ putStr $ "(" ++ s ++ ") "
	liftIO $ hFlush stdout
showLongNote :: String -> Annex ()
showLongNote s = verbose $ do
	liftIO $ putStr $ "\n" ++ (indent s)
	where
		indent s = join "\n" $ map (\l -> "  " ++ l) $ lines s 
showEndOk :: Annex ()
showEndOk = verbose $ do
	liftIO $ putStrLn "ok"
showEndFail :: Annex ()
showEndFail = verbose $ do
	liftIO $ putStrLn "\nfailed"
