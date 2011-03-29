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
	m <- Remote.readRemoteLog
	(u, c) <- case findByName name m of
		Just t -> return t
		Nothing -> do
			uuid <- liftIO $ genUUID
			return $ (uuid, M.insert nameKey name M.empty)
	return $ Just $ perform name u $ M.union config c

	where
		ws = words params
		name = head ws
		config = Remote.keyValToMap $ tail ws

perform :: String -> UUID -> M.Map String String -> CommandPerform
perform name uuid config = do
	liftIO $ putStrLn $ show $ (uuid, config)
	return Nothing

findByName :: String ->  M.Map UUID (M.Map String String) -> Maybe (UUID, M.Map String String)
findByName n m = if null matches then Nothing else Just $ head matches
	where
		matches = filter (matching . snd) $ M.toList m
		matching c = case M.lookup nameKey c of
			Nothing -> False
			Just n'
				| n' == n -> True
				| otherwise -> False

{- The name of a configured remote is stored in its config using this key. -}
nameKey :: String
nameKey = "name"
