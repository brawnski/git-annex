{- git-annex v2 -> v2 upgrade support
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Upgrade.V1 where

import System.IO.Error (try)
import System.Directory
import Control.Monad.State (liftIO)
import Control.Monad (filterM, forM_, unless)
import System.Posix.Files
import System.FilePath
import Data.String.Utils
import System.Posix.Types
import Data.Maybe
import Data.Char

import Types.Key
import Content
import Types
import Locations
import LocationLog
import qualified Annex
import qualified AnnexQueue
import qualified GitRepo as Git
import Backend
import Messages
import Version
import Utility
import qualified Command.Init

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
