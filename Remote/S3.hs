{- Amazon S3 remotes.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.S3 (remote) where

import Network.AWS.AWSConnection
import Network.AWS.S3Object
import Network.AWS.S3Bucket
import Network.AWS.AWSResult
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M
import Data.String.Utils
import Control.Monad (filterM, liftM, when)
import Control.Monad.State (liftIO)
import System.Environment
import Data.Char
import Messages

import RemoteClass
import Types
import qualified GitRepo as Git
import qualified Annex
import UUID
import Config

remote :: RemoteType Annex
remote = RemoteType {
	typename = "S3",
	generator = gen,
	setup = s3Setup
}

gen :: Annex (RemoteGenerator Annex)
gen = do
	g <- Annex.gitRepo
	remotes <- filterM remoteNotIgnored $ findS3Remotes g
	todo <- filterM cachedUUID remotes
	let ok = filter (`notElem` todo) remotes
	
	let actions = map (\r -> genRemote r =<< getUUID r) ok ++
		map (\r -> genRemote r =<< getS3UUID r) todo
	return (actions, map Git.repoDescribe todo)

	where
		cachedUUID r = liftM null $ getUUID r

{- S3 remotes have a remote.<name>.annex-s3-bucket config setting.
 - Git.Repo does not normally generate remotes for things that
 - have no configured url, so the Git.Repo objects have to be
 - constructed as coming from an unknown location. -}
findS3Remotes :: Git.Repo -> [Git.Repo]
findS3Remotes r = map construct remotepairs
	where
		remotepairs = M.toList $ filterremotes $ Git.configMap r
		filterremotes = M.filterWithKey (\k _ -> s3remote k)
		construct (k,_) = Git.repoRemoteNameSet Git.repoFromUnknown k
		s3remote k = startswith "remote." k && endswith ".annex-s3-bucket" k

genRemote :: Git.Repo -> UUID -> Annex (Remote Annex)
genRemote r u = do
	c <- remoteCost r
	return Remote {
		uuid = u,
		cost = c,
		name = Git.repoDescribe r,
		storeKey = error "TODO",
		retrieveKeyFile = error "TODO",
		removeKey = error "TODO",
		hasKey = error "TODO",
		hasKeyCheap = False,
		config = Nothing
	}

s3Connection :: Git.Repo -> Annex (Maybe AWSConnection)
s3Connection r = do
	host <- getS3Config r "s3-host" (Just defaultAmazonS3Host)
	port <- getS3Config r "s3-port" (Just $ show defaultAmazonS3Port)
	accesskey <- getS3Config r "s3-access-key-id" Nothing
	secretkey <- getS3Config r "s3-secret-access-key" Nothing
	case reads port of
		[(p, _)] -> return $ Just $ AWSConnection host p accesskey secretkey
		_ -> error $ "bad S3 port value: " ++ port

withS3Connection :: Git.Repo -> Annex a -> ((AWSConnection, String) -> Annex a) -> Annex a
withS3Connection r def a = do
	c <- s3Connection r
	case c of
		Nothing -> def
		Just c' -> do
			b <- getConfig r "s3-bucket" ""
			a (c', b)

getS3Config :: Git.Repo -> String -> Maybe String-> Annex String
getS3Config r s def = do
	e <- liftIO $ catch (liftM Just $ getEnv envvar) (const $ return def)
	v <- case e of
		Nothing -> getConfig r s ""
		Just d -> getConfig r s d
	when (null v) $ error $ "set " ++ envvar ++ " or " ++ remoteConfig r s
	return v
	where
		envvar = "ANNEX_" ++ map (\c -> if c == '-' then '_' else toUpper c) s

s3Setup :: UUID -> M.Map String String -> Annex (M.Map String String)
s3Setup u c = do
	return c

{- The UUID of a S3 bucket is stored in a file "git-annex-uuid" in the
 - bucket. Gets the UUID, or if there is none, sets a new UUID, possibly
 - also creating the bucket. -}
getS3UUID :: Git.Repo -> Annex UUID
getS3UUID r = withS3Connection r disable $ \(c, b) -> do
	res <- liftIO $
		getObject c $ S3Object b uuidfile "" [] L.empty
	case res of
		Right o -> return $ L.unpack $ obj_data o
		Left _ -> do
			location <- getS3Config r "s3-datacenter" (Just "EU")
			-- bucket may already exist, or not
			_ <- liftIO $ createBucketIn c b location
			u <- getUUID r
			res' <- liftIO $ sendObject c $
				S3Object b uuidfile "" [] $
					L.pack u
			case res' of
				Right _ -> return u
				Left e -> do
					warning $ prettyReqError e
					disable
					
	where
		uuidfile = "git-annex-uuid"
		disable = return "" -- empty uuid will disable this remote
