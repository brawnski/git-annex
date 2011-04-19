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
import Config
import Remote.Special
import Remote.Encryptable
import Crypto

remote :: RemoteType Annex
remote = RemoteType {
	typename = "S3",
	enumerate = findSpecialRemotes "s3",
	generate = gen,
	setup = s3Setup
}

gen :: Git.Repo -> UUID -> Maybe RemoteConfig -> Annex (Remote Annex)
gen r u c = do
	cst <- remoteCost r expensiveRemoteCost
	return $ gen' r u c cst
gen' :: Git.Repo -> UUID -> Maybe RemoteConfig -> Int -> Remote Annex
gen' r u c cst =
	encryptableRemote c
		(storeEncrypted this)
		(retrieveEncrypted this)
		this
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

s3ConnectionRequired :: RemoteConfig -> Annex AWSConnection
s3ConnectionRequired c = do
	conn <- s3Connection c
	case conn of
		Nothing -> error "Cannot connect to S3"
		Just conn' -> return conn'

s3Connection :: RemoteConfig -> Annex (Maybe AWSConnection)
s3Connection c = do
	ak <- getEnvKey "AWS_ACCESS_KEY_ID"
	sk <- getEnvKey "AWS_SECRET_ACCESS_KEY"
	if (null ak || null sk)
		then do
			warning "Set both AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY to use S3"
			return Nothing
		else return $ Just $ AWSConnection host port ak sk
	where
		host = fromJust $ (M.lookup "host" c)
		port = let s = fromJust $ (M.lookup "port" c) in
			case reads s of
			[(p, _)] -> p
			_ -> error $ "bad S3 port value: " ++ s
		getEnvKey s = liftIO $ catch (getEnv s) (const $ return "")

s3Setup :: UUID -> RemoteConfig -> Annex RemoteConfig
s3Setup u c = do
	-- verify configuration is sane
	c' <- encryptionSetup c
	let fullconfig = M.union c' defaults

	-- check bucket location to see if the bucket exists, and create it
	let datacenter = fromJust $ M.lookup "datacenter" fullconfig
	conn <- s3ConnectionRequired fullconfig
	
	showNote "checking bucket"
	loc <- liftIO $ getBucketLocation conn bucket 
	case loc of
		Right _ -> return ()
		Left err@(NetworkError _) -> s3Error err
		Left (AWSError _ _) -> do
			showNote $ "creating bucket in " ++ datacenter
			res <- liftIO $ createBucketIn conn bucket datacenter
			case res of
				Right _ -> return ()
				Left err -> s3Error err

	gitConfigSpecialRemote u fullconfig "s3" "true"
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

s3Action :: Remote Annex -> a -> ((AWSConnection, String) -> Annex a) -> Annex a
s3Action r noconn action = do
	when (config r == Nothing) $
		error $ "Missing configuration for special remote " ++ name r
	let bucket = M.lookup "bucket" $ fromJust $ config r
	conn <- s3Connection (fromJust $ config r)
	case (bucket, conn) of
		(Just b, Just c) -> action (c, b)
		_ -> return noconn

bucketKey :: String -> Key -> L.ByteString -> S3Object
bucketKey bucket k content = S3Object bucket (show k) "" [] content

checkPresent :: Remote Annex -> Key -> Annex (Either IOException Bool)
checkPresent r k = s3Action r noconn $ \(conn, bucket) -> do
	showNote ("checking " ++ name r ++ "...")
	res <- liftIO $ getObjectInfo conn $ bucketKey bucket k L.empty
	case res of
		Right _ -> return $ Right True
		Left (AWSError _ _) -> return $ Right False
		Left e -> return $ Left (s3Error e)
	where
		noconn = Left $ error "S3 not configured"

store :: Remote Annex -> Key -> Annex Bool
store r k = s3Action r False $ \(conn, bucket) -> do
	content <- lazyKeyContent k
	res <- liftIO $ storeHelper (conn, bucket) r k content
	s3Bool res

storeEncrypted :: Remote Annex -> (Cipher, Key) -> Key -> Annex Bool
storeEncrypted r (cipher, enck) k = s3Action r False $ \(conn, bucket) -> do
	content <- lazyKeyContent k
	res <- liftIO $ withEncryptedContent cipher content $ \s -> do
		storeHelper (conn, bucket) r enck s
	s3Bool res

lazyKeyContent :: Key -> Annex L.ByteString
lazyKeyContent k = do
	g <- Annex.gitRepo
	liftIO $ L.readFile $ gitAnnexLocation g k

storeHelper :: (AWSConnection, String) -> Remote Annex -> Key -> L.ByteString -> IO (AWSResult ())
storeHelper (conn, bucket) r k content = do
	let object = setStorageClass storageclass $ bucketKey bucket k content
	sendObject conn object
	where
		storageclass =
			case fromJust $ M.lookup "storageclass" $ fromJust $ config r of
				"REDUCED_REDUNDANCY" -> REDUCED_REDUNDANCY
				_ -> STANDARD

retrieve :: Remote Annex -> Key -> FilePath -> Annex Bool
retrieve r k f = s3Action r False $ \(conn, bucket) -> do
	res <- liftIO $ getObject conn $ bucketKey bucket k L.empty
	case res of
		Right o -> do
			liftIO $ L.writeFile f $ obj_data o
			return True
		Left e -> s3Warning e

retrieveEncrypted :: Remote Annex -> (Cipher, Key) -> FilePath -> Annex Bool
retrieveEncrypted r (cipher, enck) f = s3Action r False $ \(conn, bucket) -> do
	res <- liftIO $ getObject conn $ bucketKey bucket enck L.empty
	case res of
		Right o -> liftIO $ 
			withDecryptedContent cipher (obj_data o) $ \content -> do
				L.writeFile f content
				return True
		Left e -> s3Warning e

remove :: Remote Annex -> Key -> Annex Bool
remove r k = s3Action r False $ \(conn, bucket) -> do
	res <- liftIO $ deleteObject conn $ bucketKey bucket k L.empty
	case res of
		Right _ -> return True
		Left e -> s3Warning e
			
s3Warning :: ReqError -> Annex Bool
s3Warning e = do
	warning $ prettyReqError e
	return False

s3Error :: ReqError -> a
s3Error e = error $ prettyReqError e

s3Bool :: AWSResult () -> Annex Bool
s3Bool res = do
	case res of
		Right _ -> return True
		Left e -> s3Warning e
