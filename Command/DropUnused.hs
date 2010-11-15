{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.DropUnused where

import Control.Monad.State (liftIO)
import qualified Data.Map as M

import Command
import Types
import Messages
import Locations
import qualified Annex
import qualified Command.Drop
import Backend

seek :: [SubCmdSeek]
seek = [withStrings start]

{- Drops unused content by number. -} 
start :: SubCmdStartString
start s = do
	m <- readUnusedLog
	case M.lookup s m of
		Nothing -> return Nothing
		Just key -> do
			showStart "dropunused" s
			backend <- keyBackend key
			-- force drop, even if this is the only copy
			Annex.flagChange "force" $ FlagBool True
			return $ Just $ Command.Drop.perform key backend

readUnusedLog :: Annex (M.Map String Key)
readUnusedLog = do
	g <- Annex.gitRepo
	l <- liftIO $ readFile (annexUnusedLog g)
	return $ M.fromList $ map parse $ lines l
	where
		parse line = (head ws, tokey $ unwords $ tail ws)
			where
				ws = words line
				tokey s = read s :: Key
