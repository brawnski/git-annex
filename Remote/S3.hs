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
import Data.Maybe
import Data.String.Utils
import Control.Monad (filterM, liftM, when)
import Control.Monad.State (liftIO)
import System.Environment
import Data.Char

import RemoteClass
import Types
import qualified GitRepo as Git
import qualified Annex
import UUID
import Config
import Utility
import Messages

remote :: RemoteType Annex
remote = RemoteType {
	typename = "S3",
	generator = gen,
	setup = s3Setup
}

gen :: Annex [Remote Annex]
gen = do
	g <- Annex.gitRepo
	l <- filterM remoteNotIgnored $ findS3Remotes g
	generated <- mapM genRemote l
	return $ catMaybes generated

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

genRemote :: Git.Repo -> Annex (Maybe (Remote Annex))
genRemote r = do
	u <- getUUID r
	if (u == "")
		then return Nothing
		else do
			c <- remoteCost r
			return $ Just $ Remote {
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
		Just k -> error "encryption keys not yet supported"
	let fullconfig = M.union c defaults

	-- check bucket location to see if the bucket exists
	let datacenter = fromJust $ M.lookup "datacenter" fullconfig
	conn <- liftIO $ s3Connection fullconfig
	showNote "checking bucket"
	loc <- liftIO $ getBucketLocation conn bucket 
	case loc of
		Right _ -> return ()
		Left err@(NetworkError _) -> error $ prettyReqError err
		Left (AWSError _ _) -> do
			showNote "creating bucket"
			res <- liftIO $ createBucketIn conn bucket datacenter
			case res of
				Right _ -> return ()
				Left err -> error $ prettyReqError err

	g <- Annex.gitRepo
	liftIO $ do
		Git.run g "config" [Param ("remote." ++ name ++ ".annex-s3"), Param "true"]
		Git.run g "config" [Param ("remote." ++ name ++ ".annex-uuid"), Param u]
	return fullconfig
	where
		name = fromJust (M.lookup "name" c)
		bucket = name ++ "-" ++ u
		defaults = M.fromList
			[ ("datacenter", "US")
			, ("storageclass", "STANDARD")
			, ("host", defaultAmazonS3Host)
			, ("port", show defaultAmazonS3Port)
			, ("bucket", bucket)
			]

{-

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

-}
