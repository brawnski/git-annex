{- git-annex v2 -> v2 upgrade support
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Upgrade.V2 where

import System.Directory
import System.FilePath

import Types.Key
import Types
import qualified GitRepo as Git
import Messages
import Utility
import Locations

upgrade :: Annex Bool
upgrade = do
	showNote "v2 to v3"
	error "TODO"

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
		safeWriteFile attributes $ unlines $
			filter (\l -> not $ l `elem` attrLines) $ lines c

oldlogFile :: Git.Repo -> Key -> String
oldlogFile = logFile' hashDirLower

oldlogFileOld :: Git.Repo -> Key -> String
oldlogFileOld = logFile' hashDirMixed

logFile' :: (Key -> FilePath) -> Git.Repo -> Key -> String
logFile' hasher repo key =
	gitStateDir repo ++ hasher key ++ keyFile key ++ ".log"
