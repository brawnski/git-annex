{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.DropUnused where

import Control.Monad (when)
import Control.Monad.State (liftIO)
import qualified Data.Map as M
import System.Directory
import Data.Maybe

import Command
import Types
import Messages
import Locations
import qualified Annex
import qualified Command.Drop
import Backend
import Key

command :: [Command]
command = [repoCommand "dropunused" (paramRepeating paramNumber) seek
	"drop unused file content"]

seek :: [CommandSeek]
seek = [withStrings start]

{- Drops unused content by number. -} 
start :: CommandStartString
start s = notBareRepo $ do
	m <- readUnusedLog
	case M.lookup s m of
		Nothing -> return Nothing
		Just key -> do
			g <- Annex.gitRepo
			showStart "dropunused" s
			backend <- keyBackend key
			-- drop both content in the backend and any tmp
			-- file for the key
			let tmp = gitAnnexTmpLocation g key
			tmp_exists <- liftIO $ doesFileExist tmp
			when tmp_exists $ liftIO $ removeFile tmp
			return $ Just $ Command.Drop.perform key backend (Just 0)

readUnusedLog :: Annex (M.Map String Key)
readUnusedLog = do
	g <- Annex.gitRepo
	let f = gitAnnexUnusedLog g
	e <- liftIO $ doesFileExist f
	if e
		then do
			l <- liftIO $ readFile f
			return $ M.fromList $ map parse $ lines l
		else return $ M.empty
	where
		parse line = (head ws, fromJust $ readKey $ unwords $ tail ws)
			where
				ws = words line
