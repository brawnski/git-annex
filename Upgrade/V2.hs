{- git-annex v2 -> v3 upgrade support
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Upgrade.V2 where

import System.Directory
import System.FilePath
import Control.Monad.State (liftIO)
import List
import Data.Maybe

import Types.Key
import Types
import qualified Annex
import qualified GitRepo as Git
import qualified Branch
import Messages
import Utility
import Locations

olddir :: FilePath
olddir = ".git-annex"

{- .git-annex/ moved to a git-annex branch.
 - 
 - Strategy:
 - 
 - * Create the git-annex branch.
 - * Find each location log file in .git-annex/, and inject its content
 -   into the git-annex branch, unioning with any content already in
 -   there. (in passing, this deals with the semi transition that left
 -   some location logs hashed two different ways; both are found and
 -   merged).
 - * Also inject remote.log, trust.log, and uuid.log.
 - * git rm -rf .git-annex
 - * Remove stuff that used to be needed in .gitattributes.
 - * Commit changes.
 -}
upgrade :: Annex Bool
upgrade = do
	showNote "v2 to v3"
	g <- Annex.gitRepo
	Branch.create
	mapM_ (\(k, f) -> inject f $ logFile k) =<< locationLogs g
	mapM_ (\f -> inject f f) =<< logFiles olddir
	liftIO $ do
		Git.run g "rm" [Param "-r", Param "-f", Param "-q", File olddir]
		gitAttributesUnWrite g

	showLongNote $
		"git-annex branch created\n" ++
		"Now you should push the new branch: git push origin git-annex\n"

	return True

locationLogs :: Git.Repo -> Annex [(Key, FilePath)]
locationLogs repo = liftIO $ do
	levela <- dirContents dir
	levelb <- mapM tryDirContents levela
	files <- mapM tryDirContents (concat levelb)
	return $ catMaybes $ map islogfile (concat files)
	where
		tryDirContents d = catch (dirContents d) (return . const [])
		dir = gitStateDir repo
		islogfile f = maybe Nothing (\k -> Just $ (k, f)) $
				logFileKey $ takeFileName f

inject :: FilePath -> FilePath -> Annex ()
inject source dest = do
	new <- liftIO (readFile $ olddir </> source)
	prev <- Branch.get dest
	Branch.change dest $ unlines $ nub $ lines prev ++ lines new

logFiles :: FilePath -> Annex [FilePath]
logFiles dir = return . filter (".log" `isSuffixOf`)
		=<< liftIO (getDirectoryContents dir)

{- Old .gitattributes contents, not needed anymore. -}
attrLines :: [String]
attrLines =
	[ stateDir </> "*.log merge=union"
	, stateDir </> "*/*/*.log merge=union"
	]

gitAttributesUnWrite :: Git.Repo -> IO ()
gitAttributesUnWrite repo = do
	let attributes = Git.attributes repo
	whenM (doesFileExist attributes) $ do
		c <- readFileStrict attributes
		liftIO $ safeWriteFile attributes $ unlines $
			filter (\l -> not $ l `elem` attrLines) $ lines c
		Git.run repo "add" [File attributes]

stateDir :: FilePath
stateDir = addTrailingPathSeparator $ ".git-annex"
gitStateDir :: Git.Repo -> FilePath
gitStateDir repo = addTrailingPathSeparator $ Git.workTree repo </> stateDir

