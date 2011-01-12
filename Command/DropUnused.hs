{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.DropUnused where

import Control.Monad.State (liftIO)
import qualified Data.Map as M
import System.Directory

import Command
import Types
import Messages
import Locations
import qualified Annex
import qualified Command.Drop
import Backend

command :: [Command]
command = [Command "dropunused" (paramRepeating paramNumber) seek
	"drop unused file content"]

seek :: [CommandSeek]
seek = [withStrings start]

{- Drops unused content by number. -} 
start :: CommandStartString
start s = do
	m <- readUnusedLog
	case M.lookup s m of
		Nothing -> return Nothing
		Just key -> do
			showStart "dropunused" s
			backend <- keyBackend key
			return $ Just $ Command.Drop.perform key backend (Just 0)

readUnusedLog :: Annex (M.Map String Key)
readUnusedLog = do
	g <- Annex.gitRepo
	let f = annexUnusedLog g
	e <- liftIO $ doesFileExist f
	if e
		then do
			l <- liftIO $ readFile f
			return $ M.fromList $ map parse $ lines l
		else return $ M.empty
	where
		parse line = (head ws, tokey $ unwords $ tail ws)
			where
				ws = words line
				tokey s = read s :: Key
