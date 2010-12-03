{- git-annex repository versioning
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Version where

import Control.Monad.State (liftIO)
import System.Directory

import Types
import qualified Annex
import qualified GitRepo as Git
import Locations

currentVersion :: String
currentVersion = "1"

versionField :: String
versionField = "annex.version"

getVersion :: Annex (Maybe String)
getVersion = do
	g <- Annex.gitRepo
	let v = Git.configGet g versionField ""
	if not $ null v
		then return $ Just v
		else do
			-- version 0 was not recorded in .git/config;
			-- such a repo should have an annexDir but no
			-- annexObjectDir
			d <- liftIO $ doesDirectoryExist $ annexDir g
			o <- liftIO $ doesDirectoryExist $ annexObjectDir g
			if d && not o
				then return $ Just "0"
				else return Nothing -- no version yet

setVersion :: Annex ()
setVersion = Annex.setConfig versionField currentVersion
