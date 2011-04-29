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
import qualified Command.Move
import qualified Remote
import qualified GitRepo as Git
import Backend
import Key

type UnusedMap = M.Map String Key

command :: [Command]
command = [repoCommand "dropunused" (paramRepeating paramNumber) seek
	"drop unused file content"]

seek :: [CommandSeek]
seek = [withUnusedMaps]

{- Read unused logs once, and pass the maps to each start action. -}
withUnusedMaps :: CommandSeek
withUnusedMaps params = do
	unused <- readUnusedLog ""
	unusedbad <- readUnusedLog "bad"
	unusedtmp <- readUnusedLog "tmp"
	return $ map (start (unused, unusedbad, unusedtmp)) params

start :: (UnusedMap, UnusedMap, UnusedMap) -> CommandStartString
start (unused, unusedbad, unusedtmp) s = notBareRepo $ search
	[ (unused, perform)
	, (unusedbad, performOther gitAnnexBadLocation)
	, (unusedtmp, performOther gitAnnexTmpLocation)
	]
	where
		search [] = return Nothing
		search ((m, a):rest) = do
			case M.lookup s m of
				Nothing -> search rest
				Just key -> do
					showStart "dropunused" s
					return $ Just $ a key

perform :: Key -> CommandPerform
perform key = do
	from <- Annex.getState Annex.fromremote
	case from of
		Just name -> do
			r <- Remote.byName name
			showNote $ "from " ++ Remote.name r ++ "..."
			return $ Just $ Command.Move.fromCleanup r True key
		_ -> do
			backend <- keyBackend key
			Command.Drop.perform key backend (Just 0) -- force drop

performOther :: (Git.Repo -> Key -> FilePath) -> Key -> CommandPerform
performOther filespec key = do
	g <- Annex.gitRepo
	let f = filespec g key
	e <- liftIO $ doesFileExist f
	when e $ liftIO $ removeFile f
	return $ Just $ return True

readUnusedLog :: FilePath -> Annex UnusedMap
readUnusedLog prefix = do
	g <- Annex.gitRepo
	let f = gitAnnexUnusedLog prefix g
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
