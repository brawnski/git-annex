{- Amazon S3 remotes.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.S3 (generate) where

import qualified Data.Map as Map
import Data.String.Utils
import Control.Monad (filterM, liftM)

import RemoteClass
import Types
import qualified GitRepo as Git
import qualified Annex
import UUID
import Config

generate :: Annex (RemoteGenerator Annex)
generate = do
	g <- Annex.gitRepo
	remotes <- filterM remoteNotIgnored $ findS3Remotes g
	todo <- filterM cachedUUID remotes
	let ok = filter (`notElem` todo) remotes
	
	let actions = map genRemote ok ++
		map (\r -> genRemote =<< tryS3ConfigRead r) todo
	return (actions, map Git.repoDescribe todo)

	where
		cachedUUID r = liftM null $ getUUID r

genRemote :: Git.Repo -> Annex (Remote Annex)
genRemote r = do
	return Remote {
		uuid = error "TODO",
		cost = error "TODO",
		name = Git.repoDescribe r,
		storeKey = error "TODO",
		retrieveKeyFile = error "TODO",
		removeKey = error "TODO",
		hasKey = error "TODO",
		hasKeyCheap = False
	}

{- S3 remotes have a remote.<name>.annex-s3-bucket config setting.
 - Git.Repo does not normally generate remotes for things that
 - have no configured url, so the Git.Repo objects have to be
 - constructed as coming from an unknown location. -}
findS3Remotes :: Git.Repo -> [Git.Repo]
findS3Remotes r = map construct remotepairs
	where
		remotepairs = Map.toList $ filterremotes $ Git.configMap r
		filterremotes = Map.filterWithKey (\k _ -> s3remote k)
		construct (k,_) = Git.repoRemoteNameSet Git.repoFromUnknown k
		s3remote k = startswith "remote." k && endswith ".annex-s3-bucket" k

tryS3ConfigRead :: Git.Repo -> Annex Git.Repo
tryS3ConfigRead r = error "TODO"
