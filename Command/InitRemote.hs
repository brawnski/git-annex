{- git-annex command
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.InitRemote where

import qualified Data.Map as M
import Control.Monad (when)
import Control.Monad.State (liftIO)

import Command
import qualified Remote
import UUID
import Messages

command :: [Command]
command = [repoCommand "initremote"
	(paramPair paramName $
		paramOptional $ paramRepeating $ paramKeyValue) seek
	"sets up a special (non-git) remote"]

seek :: [CommandSeek]
seek = [withString start]

start :: CommandStartString
start params = notBareRepo $ do
	when (null ws) $ error "Specify a name for the remote"
	showStart "initremote" name
	r <- Remote.configGet name
	(u, c) <- case r of
		Just t -> return t
		Nothing -> do
			uuid <- liftIO $ genUUID
			return $ (uuid, M.empty)
	return $ Just $ perform name u $ M.union config c

	where
		ws = words params
		name = head ws
		config = Remote.keyValToMap $ tail ws

perform :: String -> UUID -> M.Map String String -> CommandPerform
perform name uuid config = do
	liftIO $ putStrLn $ show $ (uuid, config)
	return Nothing
