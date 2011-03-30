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
import Control.Monad (when)
import Control.Monad.State (liftIO)
import System.Environment

import RemoteClass
import Types
import qualified GitRepo as Git
import qualified Annex
import UUID
import Messages
import Locations
import Remote.Special

remote :: RemoteType Annex
remote = RemoteType {
	typename = "S3",
	enumerate = findSpecialRemotes "s3",
	generate = gen,
	setup = s3Setup
}

gen :: Git.Repo -> UUID -> Cost -> Maybe (M.Map String String) -> Annex (Remote Annex)
gen r u cst c = return this
	where
		this = Remote {
			uuid = u,
			cost = cst,
			name = Git.repoDescribe r,
	 		storeKey = store this,
			retrieveKeyFile = retrieve this,
			removeKey = remove this,
			hasKey = checkPresent this,
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

	gitConfigSpecialRemote "s3" u fullconfig
	return fullconfig
	where
		remotename = fromJust (M.lookup "name" c)
		bucket = remotename ++ "-" ++ u
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

checkPresent :: Remote Annex -> Key -> Annex (Either IOException Bool)
checkPresent r k = s3Action r $ \(conn, bucket) -> do
	showNote ("checking " ++ name r ++ "...")
	res <- liftIO $ getObjectInfo conn $ bucketKey bucket k L.empty
	case res of
		Right _ -> return $ Right True
		Left (AWSError _ _) -> return $ Right False
		Left e -> return $ Left (error $ prettyReqError e)

store :: Remote Annex -> Key -> Annex Bool
store r k = s3Action r $ \(conn, bucket) -> do
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

retrieve :: Remote Annex -> Key -> FilePath -> Annex Bool
retrieve r k f = s3Action r $ \(conn, bucket) -> do
	res <- liftIO $ getObject conn $ bucketKey bucket k L.empty
	case res of
		Right o -> do
			liftIO $ L.writeFile f (obj_data o)
			return True
		Left e -> do
			warning $ prettyReqError e
			return False

remove :: Remote Annex -> Key -> Annex Bool
remove r k = s3Action r $ \(conn, bucket) -> do
	res <- liftIO $ deleteObject conn $ bucketKey bucket k L.empty
	case res of
		Right _ -> return True
		Left e -> do
			warning $ prettyReqError e
			return False