{- Amazon S3 remotes.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.S3 (remote) where

import Control.Exception.Extensible (IOException)
import Network.AWS.AWSConnection
import Network.AWS.S3Object
import Network.AWS.S3Bucket
import Network.AWS.AWSResult
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M
import Data.Maybe
import Data.String.Utils
import Control.Monad (filterM, when)
import Control.Monad.State (liftIO)
import System.Environment

import RemoteClass
import Types
import qualified GitRepo as Git
import qualified Annex
import UUID
import Config
import Utility
import Messages
import Locations

remote :: RemoteType Annex
remote = RemoteType {
	typename = "S3",
	enumerate = s3List,
	generate = s3Gen,
	setup = s3Setup
}

s3List :: Annex [Git.Repo]
s3List = do
	g <- Annex.gitRepo
	filterM remoteNotIgnored $ findS3Remotes g

{- S3 remotes have a remote.<name>.annex-s3 config setting.
 - Git.Repo does not normally generate remotes for things that
 - have no configured url, so the Git.Repo objects have to be
 - constructed as coming from an unknown location. -}
findS3Remotes :: Git.Repo -> [Git.Repo]
findS3Remotes r = map construct remotepairs
	where
		remotepairs = M.toList $ filterremotes $ Git.configMap r
		filterremotes = M.filterWithKey (\k _ -> s3remote k)
		construct (k,_) = Git.repoRemoteNameSet Git.repoFromUnknown k
		s3remote k = startswith "remote." k && endswith ".annex-s3" k

s3Gen :: Git.Repo -> Maybe (M.Map String String) -> Annex (Remote Annex)
s3Gen r c = do
	u <- getUUID r
	cst <- remoteCost r
	return $ genRemote r u c cst
	where

genRemote :: Git.Repo -> UUID -> Maybe (M.Map String String) -> Int -> Remote Annex
genRemote r u c cst = this
	where
		this = Remote {
			uuid = u,
			cost = cst,
			name = Git.repoDescribe r,
	 		storeKey = s3Store this,
			retrieveKeyFile = s3Retrieve this,
			removeKey = error "TODO removekey",
			hasKey = s3CheckPresent this,
			hasKeyCheap = False,
			config = c
		}

s3Connection :: M.Map String String -> IO AWSConnection
s3Connection c = do
	ak <- getEnvKey "AWS_ACCESS_KEY_ID"
	sk <- getEnvKey "AWS_SECRET_ACCESS_KEY"
	return $ AWSConnection host port ak sk
	where
		host = fromJust $ (M.lookup "host" c)
		port = let s = fromJust $ (M.lookup "port" c) in
			case reads s of
			[(p, _)] -> p
			_ -> error $ "bad S3 port value: " ++ s
		getEnvKey s = catch (getEnv s) (error $ "Set " ++ s)

s3Setup :: UUID -> M.Map String String -> Annex (M.Map String String)
s3Setup u c = do
	-- verify configuration is sane
	case M.lookup "encryption" c of
		Nothing -> error "Specify encryption=key or encryption=none"
		Just "none" -> return ()
		Just _ -> error "encryption keys not yet supported"
	let fullconfig = M.union c defaults

	-- check bucket location to see if the bucket exists, and create it
	let datacenter = fromJust $ M.lookup "datacenter" fullconfig
	conn <- liftIO $ s3Connection fullconfig
	showNote "checking bucket"
	loc <- liftIO $ getBucketLocation conn bucket 
	case loc of
		Right _ -> return ()
		Left err@(NetworkError _) -> error $ prettyReqError err
		Left (AWSError _ _) -> do
			showNote $ "creating bucket in " ++ datacenter
			res <- liftIO $ createBucketIn conn bucket datacenter
			case res of
				Right _ -> return ()
				Left err -> error $ prettyReqError err

	g <- Annex.gitRepo
	liftIO $ do
		Git.run g "config" [Param (configsetting "annex-s3"), Param "true"]
		Git.run g "config" [Param (configsetting "annex-uuid"), Param u]
	return fullconfig
	where
		remotename = fromJust (M.lookup "name" c)
		bucket = remotename ++ "-" ++ u
		configsetting s = "remote." ++ remotename ++ "." ++ s
		defaults = M.fromList
			[ ("datacenter", "US")
			, ("storageclass", "STANDARD")
			, ("host", defaultAmazonS3Host)
			, ("port", show defaultAmazonS3Port)
			, ("bucket", bucket)
			]

s3Action :: Remote Annex -> ((AWSConnection, String) -> Annex a) -> Annex a
s3Action r a = do
	when (config r == Nothing) $
		error $ "Missing configuration for special remote " ++ name r
	conn <- liftIO $ s3Connection (fromJust $ config r)
	let bucket = fromJust $ M.lookup "bucket" $ fromJust $ config r
	a (conn, bucket)

bucketKey :: String -> Key -> L.ByteString -> S3Object
bucketKey bucket k content = S3Object bucket (show k) "" [] content

s3CheckPresent :: Remote Annex -> Key -> Annex (Either IOException Bool)
s3CheckPresent r k = s3Action r $ \(conn, bucket) -> do
	showNote ("checking " ++ name r ++ "...")
	res <- liftIO $ getObjectInfo conn $ bucketKey bucket k L.empty
	case res of
		Right _ -> return $ Right True
		Left (AWSError _ _) -> return $ Right False
		Left e -> return $ Left (error $ prettyReqError e)

s3Store :: Remote Annex -> Key -> Annex Bool
s3Store r k = s3Action r $ \(conn, bucket) -> do
	g <- Annex.gitRepo
	content <- liftIO $ L.readFile $ gitAnnexLocation g k
	let object = setStorageClass storageclass $ bucketKey bucket k content
	res <- liftIO $ sendObject conn object
	case res of
		Right _ -> return True
		Left e -> do
			warning $ prettyReqError e
			return False
	where
		storageclass =
			case fromJust $ M.lookup "storageclass" $ fromJust $ config r of
				"REDUCED_REDUNDANCY" -> REDUCED_REDUNDANCY
				_ -> STANDARD

s3Retrieve :: Remote Annex -> Key -> FilePath -> Annex Bool
s3Retrieve r k f = s3Action r $ \(conn, bucket) -> do
	res <- liftIO $ getObject conn $ bucketKey bucket k L.empty
	case res of
		Right o -> do
			liftIO $ L.writeFile f (obj_data o)
			return True
		Left e -> do
			warning $ prettyReqError e
			return False
