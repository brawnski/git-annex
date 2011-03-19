{- git-annex repository versioning
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Version where

import Control.Monad.State (liftIO)
import Control.Monad (unless)
import System.Directory

import Types
import qualified Annex
import qualified GitRepo as Git
import Locations

type Version = String

defaultVersion :: Version
defaultVersion = "2"

supportedVersions :: [Version]
supportedVersions = [defaultVersion]

upgradableVersions :: [Version]
upgradableVersions = ["0", "1"]

versionField :: String
versionField = "annex.version"

getVersion :: Annex Version
getVersion = do
	g <- Annex.gitRepo
	let v = Git.configGet g versionField ""
	if not $ null v
		then return v
		else do
			-- version 0 was not recorded in .git/config;
			-- such a repo should have an gitAnnexDir but no
			-- gitAnnexObjectDir.
			--
			-- version 1 may not be recorded if the user
			-- forgot to init. Such a repo should have a
			-- gitAnnexObjectDir already.
			d <- liftIO $ doesDirectoryExist $ gitAnnexDir g
			o <- liftIO $ doesDirectoryExist $ gitAnnexObjectDir g
			case (d, o) of
				(True, False) -> return "0"
				(True, True) -> return "1"
				_ -> do
					setVersion
					return defaultVersion

setVersion :: Annex ()
setVersion = Annex.setConfig versionField defaultVersion

checkVersion :: Annex ()
checkVersion = do
	v <- getVersion
	unless (v `elem` supportedVersions) $ do
		error $ "Repository version " ++ v ++ 
			" is not supported. " ++
			msg v
	where
		msg v
			| v `elem` upgradableVersions = "Upgrade this repository: git-annex upgrade"
			| otherwise = "Upgrade git-annex."
