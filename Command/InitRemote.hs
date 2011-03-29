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
import qualified Annex
import qualified Remote
import qualified RemoteClass
import qualified GitRepo as Git
import Utility
import Types
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

	(u, c) <- findByName name
	let fullconfig = M.union config c	
	t <- findType fullconfig
	
	showStart "initremote" name
	return $ Just $ perform t u $ M.union config c

	where
		ws = words params
		name = head ws
		config = Remote.keyValToMap $ tail ws

perform :: RemoteClass.RemoteType Annex -> UUID -> M.Map String String -> CommandPerform
perform t u c = do
	c' <- RemoteClass.setup t u c
	return $ Just $ cleanup u c'

cleanup :: UUID -> M.Map String String -> CommandCleanup
cleanup u c = do
	Remote.configSet u c
	g <- Annex.gitRepo
	logfile <- Remote.remoteLog
	liftIO $ Git.run g "add" [File logfile]
        liftIO $ Git.run g "commit"
                [ Params "-q --allow-empty -m"
                , Param "git annex initremote"
                , File logfile
                ]
        return True

{- Look up existing remote's UUID and config by name, or generate a new one -}
findByName :: String -> Annex (UUID, M.Map String String)
findByName name = do
	m <- Remote.readRemoteLog
	case findByName' name m of
		Just i -> return i
		Nothing -> do
			uuid <- liftIO $ genUUID
			return $ (uuid, M.insert nameKey name M.empty)

findByName' :: String ->  M.Map UUID (M.Map String String) -> Maybe (UUID, M.Map String String)
findByName' n m = if null matches then Nothing else Just $ head matches
	where
		matches = filter (matching . snd) $ M.toList m
		matching c = case M.lookup nameKey c of
			Nothing -> False
			Just n'
				| n' == n -> True
				| otherwise -> False

{- find the specified remote type -}
findType :: M.Map String String -> Annex (RemoteClass.RemoteType Annex)
findType config = 
	case M.lookup typeKey config of
		Nothing -> error "Specify the type of remote with type="
		Just s -> case filter (\i -> RemoteClass.typename i == s) Remote.remoteTypes of
			[] -> error $ "Unknown remote type " ++ s
			(t:_) -> return t

{- The name of a configured remote is stored in its config using this key. -}
nameKey :: String
nameKey = "name"

{- The type of a remote is stored in its config using this key. -}
typeKey :: String
typeKey = "type"
