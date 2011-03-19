{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Version where

import Control.Monad.State (liftIO)
import Data.String.Utils

import Command
import qualified SysConfig
import Version
import Upgrade

command :: [Command]
command = [Command "version" paramNothing seek "show versions"]

seek :: [CommandSeek]
seek = [withNothing start]

start :: CommandStartNothing
start = do
	liftIO $ putStrLn $ "git-annex version: " ++ SysConfig.packageversion
	v <- getVersion
	liftIO $ putStrLn $ "local repository version: " ++ v
	liftIO $ putStrLn $ "default repository version: " ++ defaultVersion
	liftIO $ putStrLn $ "supported repository versions: " ++ vs supportedVersions
	liftIO $ putStrLn $ "upgrade supported from repository versions: " ++ vs upgradableVersions
	return Nothing
	where
		vs l = join " " l
